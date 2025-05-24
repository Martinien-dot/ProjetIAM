% ===============================================
% SERVEUR WEB PROLOG - EVALUATEUR DE FIABILITE v2.2
% AVEC INTERFACE ADMIN POUR EXTENSION DE BASE
% ===============================================
:- encoding(utf8).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_json)).
:- use_module(library(pcre)).

% Configuration du serveur
:- http_handler(root(.), home_page, []).
:- http_handler(root(evaluate), evaluate_article, [method(post)]).
:- http_handler(root(api/evaluate), api_evaluate, [method(post)]).
:- http_handler(root(admin), admin_page, []).
:- http_handler(root(admin/add), admin_add, [method(post)]).
:- http_handler(root(admin/delete), admin_delete, [method(post)]).
:- http_handler(root(admin/view), admin_view, []).
:- http_handler(root(admin/export), admin_export, []).
:- http_handler(root(admin/import), admin_import, [method(post)]).

% Démarrage du serveur
start_server(Port) :-
    http_server(http_dispatch, [port(Port)]),
    format('Serveur démarré sur http://localhost:~w~n', [Port]).

% -------------------------------
% BASE DE CONNAISSANCES DYNAMIQUE ÉTENDUE
% -------------------------------
:- dynamic(reputation/2).
:- dynamic(affirmation_fiable/2).
:- dynamic(auteur_reconnu/1).
:- dynamic(mot_citation/1).
:- dynamic(mot_emotionnel/1).
:- dynamic(mot_suspect/1).
:- dynamic(mot_anonyme/1).

% Base de données initiale
reputation('le monde', 0.9).
reputation('infowars', 0.1).
reputation('science today', 0.85).
reputation('random blog', 0.3).
reputation('national geographic', 0.95).
reputation('facebook post', 0.2).
reputation('bbc', 0.88).
reputation('reuters', 0.92).
reputation('inconnu', 0.1).
reputation('anonyme', 0.1).
reputation('', 0.1).

affirmation_fiable('GIEC', 'réchauffement climatique').
affirmation_fiable('OMS', 'COVID-19').
affirmation_fiable('ONU', 'plastique').
affirmation_fiable('NASA', 'pyramides').

% Auteurs reconnus
auteur_reconnu('Jean Dupont').
auteur_reconnu('Dr. Martin').
auteur_reconnu('Prof. Durand').

% Mots-clés pour détection
mot_citation('étude').
mot_citation('recherche').
mot_citation('selon').
mot_citation('données').
mot_citation('rapport').
mot_citation('analyse').

mot_emotionnel('incroyable').
mot_emotionnel('choquant').
mot_emotionnel('scandaleux').
mot_emotionnel('urgent').
mot_emotionnel('alerte').
mot_emotionnel('révélation').

mot_suspect('mensonge').
mot_suspect('faux').
mot_suspect('démenti').
mot_suspect('complot').
mot_suspect('secret').
mot_suspect('caché').

% Mots indiquant auteur anonyme
mot_anonyme('anonyme').
mot_anonyme('inconnu').
mot_anonyme('').

% -------------------------------
% BASE DYNAMIQUE POUR EXTRACTION AUTOMATIQUE
% -------------------------------
:- dynamic(fait_extrait/3).
:- dynamic(contradiction_detectee/2).
:- dynamic(reference_croisee/2).

% Extraction automatique de faits depuis l'entrée utilisateur
extraire_faits_automatiquement(Article, Source, Auteur) :-
    downcase_atom(Article, ArticleLower),
    % Extraction de nouvelles sources mentionnées
    (sub_string(ArticleLower, _, _, _, 'selon ') ->
        assert(fait_extrait(source_mentionnee, Article, Source))
    ; true),
    
    % Extraction d'affirmations
    (sub_string(ArticleLower, _, _, _, 'affirme') ->
        assert(fait_extrait(affirmation, Article, Auteur))
    ; true),
    
    % Détection de références croisées
    (sub_string(ArticleLower, _, _, _, 'confirme') ->
        assert(reference_croisee(Article, Source))
    ; true).

% -------------------------------
% CRITÈRES D'ÉVALUATION (INCHANGÉS)
% -------------------------------

% Critère 1: Réputation de la source (25 points)
evaluate_reputation(Source, Score, Details) :-
    downcase_atom(Source, SourceLower),
    (reputation(SourceLower, Rep) ->
        (Rep >= 0.8 -> 
            Score = 25, format(atom(Details), 'Source très fiable (~2f)', [Rep])
        ; Rep >= 0.5 -> 
            Score = 20, format(atom(Details), 'Source moyennement fiable (~2f)', [Rep])
        ; 
            Score = 5, format(atom(Details), 'Source peu fiable (~2f)', [Rep])
        )
    ; 
        Score = 0, Details = 'Source inconnue',
        assert(fait_extrait(source_inconnue, Source, unknown))
    ).

% Critère 2: Évaluation de l'auteur (25 points)
evaluate_author(Auteur, Score, Details) :-
    downcase_atom(Auteur, AuteurLower),
    (mot_anonyme(AuteurLower) ->
        Score = 0, Details = 'Auteur anonyme'
    ; auteur_reconnu(Auteur) ->
        Score = 25, Details = 'Auteur reconnu et crédible'
    ; Auteur \= '' ->
        Score = 15, Details = 'Auteur identifié mais non reconnu'
    ;
        Score = 0, Details = 'Aucun auteur spécifié'
    ).

% Critère 3: Présence de citations et références (25 points)
evaluate_citations(Article, Score, Details) :-
    downcase_atom(Article, ArticleLower),
    % Détection par mots-clés
    findall(Mot, (mot_citation(Mot), sub_string(ArticleLower, _, _, _, Mot)), MotsCles),
    length(MotsCles, NbMotsCles),
    
    % Détection de citations avec guillemets
    (re_match('"[^"]+"', Article) -> 
        HasCitation = 1
    ; 
        HasCitation = 0
    ),
    
    % Vérification références croisées
    (reference_croisee(Article, _) ->
        HasCrossRef = 1
    ;
        HasCrossRef = 0
    ),
    
    % Score combiné
    TotalScore is NbMotsCles + HasCitation + HasCrossRef,
    
    (TotalScore >= 4 ->
        Score = 25, format(atom(Details), 'Références solides (~w éléments)', [TotalScore])
    ; TotalScore >= 2 ->
        Score = 15, format(atom(Details), 'Quelques références (~w éléments)', [TotalScore])
    ; TotalScore >= 1 ->
        Score = 10, format(atom(Details), 'Références minimales (~w élément)', [TotalScore])
    ;
        Score = 0, Details = 'Aucune référence détectée'
    ).

% Critère 4: Style de langage (15 points)
evaluate_style(Article, Score, Details) :-
    downcase_atom(Article, ArticleLower),
    findall(Mot, (mot_emotionnel(Mot), sub_string(ArticleLower, _, _, _, Mot)), MotsEmotionnels),
    findall(Mot, (mot_suspect(Mot), sub_string(ArticleLower, _, _, _, Mot)), MotsSuspects),
    length(MotsEmotionnels, NbEmotionnels),
    length(MotsSuspects, NbSuspects),
    
    % Calcul du score basé uniquement sur le style linguistique
    (NbSuspects > 0 ->
        Score = 0, format(atom(Details), 'Vocabulaire suspect (~w mots)', [NbSuspects])
    ; NbEmotionnels > 3 ->
        Score = 5, format(atom(Details), 'Langage très émotionnel (~w mots)', [NbEmotionnels])
    ; NbEmotionnels > 1 ->
        Score = 10, format(atom(Details), 'Langage émotionnel modéré (~w mots)', [NbEmotionnels])
    ; NbEmotionnels = 1 ->
        Score = 12, format(atom(Details), 'Langage légèrement émotionnel (~w mot)', [NbEmotionnels])
    ;
        Score = 15, Details = 'Langage neutre et factuel'
    ).

% Critère 5: Cohérence avec sources fiables (10 points)
evaluate_coherence(Article, Score, Details) :-
    downcase_atom(Article, ArticleLower),
    
    % Vérification contradictions internes
    (contradiction_detectee(Article, _) ->
        HasContradiction = 1
    ; (sub_string(ArticleLower, _, _, _, 'mais'), sub_string(ArticleLower, _, _, _, 'cependant')) ->
        HasContradiction = 1,
        assert(contradiction_detectee(Article, 'Contradiction interne détectée'))
    ;
        HasContradiction = 0
    ),
    
    % Vérification cohérence avec sources fiables reconnues
    findall(Org-Sujet, (
        affirmation_fiable(Org, Sujet),
        sub_string(ArticleLower, _, _, _, Sujet),
        (sub_string(ArticleLower, _, _, _, 'mensonge') ; 
         sub_string(ArticleLower, _, _, _, 'faux') ;
         sub_string(ArticleLower, _, _, _, 'démenti'))
    ), ContraSourcesFiables),
    length(ContraSourcesFiables, NbContraFiables),
    
    % Calcul du score basé sur la cohérence factuelle
    (NbContraFiables > 0 ->
        Score = 0, format(atom(Details), 'Contredit ~w source(s) fiable(s)', [NbContraFiables])
    ; HasContradiction = 1 ->
        Score = 3, Details = 'Contradictions internes détectées'
    ;
        Score = 10, Details = 'Cohérent avec les faits établis'
    ).

% -------------------------------
% ÉVALUATION GLOBALE (INCHANGÉE)
% -------------------------------
evaluate_credibility(Article, Source, Auteur, FinalScore, Level, RepScore, AutScore, CitScore, StyleScore, CoherenceScore, RepDetails, AutDetails, CitDetails, StyleDetails, CoherenceDetails) :-
    % Extraction automatique de faits
    extraire_faits_automatiquement(Article, Source, Auteur),
    
    % Évaluation des 5 critères séparés
    evaluate_reputation(Source, RepScore, RepDetails),
    evaluate_author(Auteur, AutScore, AutDetails),
    evaluate_citations(Article, CitScore, CitDetails),
    evaluate_style(Article, StyleScore, StyleDetails),
    evaluate_coherence(Article, CoherenceScore, CoherenceDetails),
    
    FinalScore is RepScore + AutScore + CitScore + StyleScore + CoherenceScore,
    
    % Classification sur 85 points au lieu de 100
    (FinalScore >= 70 -> Level = 'TRÈS CRÉDIBLE' ;
     FinalScore >= 55 -> Level = 'CRÉDIBLE' ;
     FinalScore >= 30 -> Level = 'DOUTEUSE' ;
     Level = 'SUSPECTE').

% -------------------------------
% INTERFACE WEB PRINCIPALE (INCHANGÉE)
% -------------------------------
home_page(_Request) :-
    reply_html_page(
        title('Évaluateur de Fiabilité'),
        [style('
            body { 
                font-family: monospace; 
                background: #f8f8f8; 
                margin: 0; 
                padding: 20px;
                color: #333;
                line-height: 1.4;
            }
            .container { 
                max-width: 600px; 
                margin: 0 auto; 
                background: white; 
                border: 1px solid #ccc; 
                padding: 20px;
            }
            h1 { 
                text-align: center; 
                background: #333; 
                color: white; 
                padding: 8px; 
                margin: -20px -20px 20px -20px;
                font-size: 16px;
                font-weight: normal;
            }
            .form-group { 
                margin: 15px 0; 
            }
            label { 
                display: block; 
                margin-bottom: 3px; 
                font-size: 12px;
                text-transform: uppercase;
            }
            input, textarea { 
                width: 100%; 
                padding: 4px; 
                border: 1px solid #999; 
                font-family: monospace;
                font-size: 11px;
                box-sizing: border-box;
            }
            button { 
                background: #eee; 
                border: 1px solid #999; 
                padding: 6px 12px; 
                font-family: monospace;
                cursor: pointer;
                margin: 10px 0;
                text-transform: uppercase;
                font-size: 11px;
            }
            button:hover { background: #ddd; }
            .info {
                border: 1px solid #999;
                background: #f5f5f5;
                padding: 8px;
                margin: 10px 0;
                font-size: 10px;
            }
            .admin-link {
                position: absolute;
                top: 10px;
                right: 10px;
                background: #333;
                color: white;
                padding: 4px 8px;
                text-decoration: none;
                font-size: 10px;
                border: 1px solid #000;
            }
            .admin-link:hover { background: #555; }
        ')],
        [
            a([href='/admin', class='admin-link'], 'ADMIN'),
            div(class=container, [
                h1('Évaluateur de Fiabilité'),
                
                form([action=evaluate, method=post], [
                    div(class='form-group', [
                        label(for=article, 'Article:'),
                        textarea([name=article, id=article, rows=6, required, 
                                 placeholder='Texte de l\'article à analyser...'], '')
                    ]),
                    
                    div(class='form-group', [
                        label(for=source, 'Source:'),
                        input([name=source, id=source, type=text, required, 
                              placeholder='Ex: Le Monde, BBC, inconnu'], [])
                    ]),
                    
                    div(class='form-group', [
                        label(for=auteur, 'Auteur:'),
                        input([name=auteur, id=auteur, type=text, 
                              placeholder='Ex: Jean Dupont, anonyme'], [])
                    ]),
                    
                    button([type=submit], 'Évaluer')
                ]),
                
                div(class=info, [
                    'Sources connues: Le Monde, BBC, Reuters, National Geographic, Science Today'
                ]),
                
                div(class=info, [
                    'Critères: Source (25) + Auteur (25) + Références (25) + Style (15) + Cohérence (10) = 100'
                ])
            ])
        ]
    ).

% -------------------------------
% INTERFACE ADMIN PRINCIPALE
% -------------------------------
admin_page(_Request) :-
    reply_html_page(
        title('Administration - Base de Faits'),
        [style('
            body { 
                font-family: monospace; 
                background: #f0f0f0; 
                margin: 0; 
                padding: 20px;
                color: #333;
                line-height: 1.4;
            }
            .container { 
                max-width: 800px; 
                margin: 0 auto; 
                background: white; 
                border: 1px solid #999; 
                padding: 20px;
            }
            h1 { 
                text-align: center; 
                background: #660000; 
                color: white; 
                padding: 8px; 
                margin: -20px -20px 20px -20px;
                font-size: 16px;
                font-weight: normal;
            }
            .section {
                border: 1px solid #999;
                margin: 15px 0;
                background: #fafafa;
            }
            .section h3 {
                background: #333;
                color: white;
                margin: 0;
                padding: 6px 10px;
                font-size: 12px;
                font-weight: normal;
                text-transform: uppercase;
            }
            .section-content {
                padding: 15px;
            }
            .form-group { 
                margin: 10px 0; 
            }
            label { 
                display: block; 
                margin-bottom: 3px; 
                font-size: 11px;
                text-transform: uppercase;
                font-weight: bold;
            }
            input, textarea, select { 
                width: 100%; 
                padding: 4px; 
                border: 1px solid #999; 
                font-family: monospace;
                font-size: 11px;
                box-sizing: border-box;
            }
            button { 
                background: #eee; 
                border: 1px solid #999; 
                padding: 6px 12px; 
                font-family: monospace;
                cursor: pointer;
                margin: 5px 5px 5px 0;
                text-transform: uppercase;
                font-size: 10px;
            }
            button:hover { background: #ddd; }
            .btn-primary { background: #006600; color: white; }
            .btn-primary:hover { background: #008800; }
            .btn-danger { background: #990000; color: white; }
            .btn-danger:hover { background: #bb0000; }
            .back-link {
                display: block;
                text-align: center;
                margin: 20px 0;
                padding: 8px;
                background: #eee;
                border: 1px solid #999;
                text-decoration: none;
                color: #333;
                font-size: 11px;
                text-transform: uppercase;
            }
            .back-link:hover { background: #ddd; }
            .info {
                background: #ffffcc;
                border: 1px solid #cccc00;
                padding: 8px;
                margin: 10px 0;
                font-size: 10px;
            }
        ')],
        [div(class=container, [
            h1('Administration - Base de Faits'),
            
            % Section Sources
            div(class=section, [
                h3('Gestion des Sources'),
                div(class='section-content', [
                    form([action='/admin/add', method=post], [
                        input([name=type, type=hidden, value=source], []),
                        div(class='form-group', [
                            label(for='source-name', 'Nom de la source:'),
                            input([name=name, id='source-name', type=text, required, 
                                  placeholder='Ex: France24'], [])
                        ]),
                        div(class='form-group', [
                            label(for='source-score', 'Score de fiabilité (0.0 à 1.0):'),
                            input([name=score, id='source-score', type=number, 
                                  min='0', max='1', step='0.01', required, 
                                  placeholder='0.85'], [])
                        ]),
                        button([type=submit, class='btn-primary'], 'Ajouter Source')
                    ])
                ])
            ]),
            
            % Section Auteurs
            div(class=section, [
                h3('Gestion des Auteurs Reconnus'),
                div(class='section-content', [
                    form([action='/admin/add', method=post], [
                        input([name=type, type=hidden, value=auteur], []),
                        div(class='form-group', [
                            label(for='auteur-name', 'Nom de l\'auteur:'),
                            input([name=name, id='auteur-name', type=text, required, 
                                  placeholder='Dr. Marie Curie'], [])
                        ]),
                        button([type=submit, class='btn-primary'], 'Ajouter Auteur')
                    ])
                ])
            ]),
            
            % Section Affirmations Fiables
            div(class=section, [
                h3('Gestion des Affirmations Fiables'),
                div(class='section-content', [
                    form([action='/admin/add', method=post], [
                        input([name=type, type=hidden, value=affirmation], []),
                        div(class='form-group', [
                            label(for='org-name', 'Organisation:'),
                            input([name=organisation, id='org-name', type=text, required, 
                                  placeholder='UNESCO'], [])
                        ]),
                        div(class='form-group', [
                            label(for='subject-name', 'Sujet d\'expertise:'),
                            input([name=sujet, id='subject-name', type=text, required, 
                                  placeholder='éducation'], [])
                        ]),
                        button([type=submit, class='btn-primary'], 'Ajouter Affirmation')
                    ])
                ])
            ]),
            
            % Section Mots-clés
            div(class=section, [
                h3('Gestion des Mots-clés'),
                div(class='section-content', [
                    form([action='/admin/add', method=post], [
                        input([name=type, type=hidden, value=mot], []),
                        div(class='form-group', [
                            label(for='mot-type', 'Type de mot:'),
                            select([name=mot_type, id='mot-type', required], [
                                option([value=citation], 'Citation/Référence'),
                                option([value=emotionnel], 'Émotionnel'),
                                option([value=suspect], 'Suspect'),
                                option([value=anonyme], 'Anonyme')
                            ])
                        ]),
                        div(class='form-group', [
                            label(for='mot-value', 'Mot-clé:'),
                            input([name=mot, id='mot-value', type=text, required, 
                                  placeholder='étude scientifique'], [])
                        ]),
                        button([type=submit, class='btn-primary'], 'Ajouter Mot-clé')
                    ])
                ])
            ]),
            
            % Section Actions
            div(class=section, [
                h3('Actions'),
                div(class='section-content', [
                    a([href='/admin/view'], button(class='btn-primary', 'Voir Toute la Base')),
                    ' ',
                    a([href='/admin/export'], button(class='btn-primary', 'Exporter')),
                    ' ',
                    form([action='/admin/delete', method=post, style='display:inline'], [
                        select([name=delete_type], [
                            option([value=all], 'Tout'),
                            option([value=sources], 'Sources'),
                            option([value=auteurs], 'Auteurs'),
                            option([value=affirmations], 'Affirmations'),
                            option([value=mots], 'Mots-clés')
                        ]),
                        button([type=submit, class='btn-danger', 
                               onclick='return confirm("Êtes-vous sûr?")'], 'Supprimer')
                    ])
                ])
            ]),
            
            div(class=info, [
                'Interface d\'administration pour étendre la base de connaissances. ',
                'Toutes les modifications sont persistantes pendant la session.'
            ]),
            
            a([href='/', class='back-link'], 'Retour à l\'évaluateur')
        ])]
    ).

% -------------------------------
% GESTION DES AJOUTS ADMIN
% -------------------------------
admin_add(Request) :-
    http_parameters(Request, Parameters),
    member(type=Type, Parameters),
    process_admin_add(Type, Parameters, Message),
    
    reply_html_page(
        title('Ajout Effectué'),
        [style('
            body { font-family: monospace; background: #f0f0f0; padding: 20px; }
            .container { max-width: 600px; margin: 0 auto; background: white; 
                        border: 1px solid #999; padding: 20px; text-align: center; }
            .success { background: #ccffcc; border: 1px solid #00cc00; padding: 10px; margin: 15px 0; }
            .error { background: #ffcccc; border: 1px solid #cc0000; padding: 10px; margin: 15px 0; }
            a { display: inline-block; margin: 10px; padding: 8px 16px; background: #eee; 
                border: 1px solid #999; text-decoration: none; color: #333; }
        ')],
        [div(class=container, [
            h2('Résultat'),
            div([class=success], Message),
            a([href='/admin'], 'Retour Admin'),
            a([href='/'], 'Accueil')
        ])]
    ).

process_admin_add(source, Parameters, Message) :-
    member(name=Name, Parameters),
    member(score=ScoreAtom, Parameters),
    atom_number(ScoreAtom, Score),
    downcase_atom(Name, NameLower),
    (reputation(NameLower, _) ->
        retract(reputation(NameLower, _))
    ; true),
    assert(reputation(NameLower, Score)),
    format(atom(Message), 'Source "~w" ajoutée avec score ~2f', [Name, Score]).

process_admin_add(auteur, Parameters, Message) :-
    member(name=Name, Parameters),
    (auteur_reconnu(Name) ->
        Message = 'Auteur déjà existant'
    ; assert(auteur_reconnu(Name)),
      format(atom(Message), 'Auteur "~w" ajouté', [Name])
    ).

process_admin_add(affirmation, Parameters, Message) :-
    member(organisation=Org, Parameters),
    member(sujet=Sujet, Parameters),
    (affirmation_fiable(Org, Sujet) ->
        Message = 'Affirmation déjà existante'
    ; assert(affirmation_fiable(Org, Sujet)),
      format(atom(Message), 'Affirmation "~w" -> "~w" ajoutée', [Org, Sujet])
    ).

process_admin_add(mot, Parameters, Message) :-
    member(mot_type=Type, Parameters),
    member(mot=Mot, Parameters),
    downcase_atom(Mot, MotLower),
    process_mot_add(Type, MotLower, Message).

process_mot_add(citation, Mot, Message) :-
    (mot_citation(Mot) ->
        Message = 'Mot-clé citation déjà existant'
    ; assert(mot_citation(Mot)),
      format(atom(Message), 'Mot-clé citation "~w" ajouté', [Mot])
    ).

process_mot_add(emotionnel, Mot, Message) :-
    (mot_emotionnel(Mot) ->
        Message = 'Mot-clé émotionnel déjà existant'
    ; assert(mot_emotionnel(Mot)),
      format(atom(Message), 'Mot-clé émotionnel "~w" ajouté', [Mot])
    ).

process_mot_add(suspect, Mot, Message) :-
    (mot_suspect(Mot) ->
        Message = 'Mot-clé suspect déjà existant'
    ; assert(mot_suspect(Mot)),
      format(atom(Message), 'Mot-clé suspect "~w" ajouté', [Mot])
    ).

process_mot_add(anonyme, Mot, Message) :-
    (mot_anonyme(Mot) ->
        Message = 'Mot-clé anonyme déjà existant'
    ; assert(mot_anonyme(Mot)),
      format(atom(Message), 'Mot-clé anonyme "~w" ajouté', [Mot])
    ).

% -------------------------------
% VISUALISATION DE LA BASE
% -------------------------------
admin_view(_Request) :-
    findall(Source-Score, reputation(Source, Score), Sources),
    findall(Auteur, auteur_reconnu(Auteur), Auteurs),
    findall(Org-Sujet, affirmation_fiable(Org, Sujet), Affirmations),
    findall(Mot, mot_citation(Mot), MotsCitation),
    findall(Mot, mot_emotionnel(Mot), MotsEmotionnels),
    findall(Mot, mot_suspect(Mot), MotsSuspects),
    findall(Mot, mot_anonyme(Mot), MotsAnonymes),
    
    reply_html_page(
        title('Base de Faits - Vue Complète'),
        [style('
            body { font-family: monospace; background: #f0f0f0; padding: 20px; }
            .container { max-width: 900px; margin: 0 auto; background: white; 
                        border: 1px solid #999; padding: 20px; }
            h1 { background: #003366; color: white; padding: 8px; margin: -20px -20px 20px -20px; 
                 text-align: center; font-size: 16px; font-weight: normal; }
            .section { margin: 20px 0; border: 1px solid #999; }
            .section h3 { background: #666; color: white; margin: 0; padding: 6px 10px; 
                         font-size: 12px; font-weight: normal; }
            .section-content { padding: 15px; }
            .data-list { list-style: none; padding: 0; margin: 0; }
            .data-list li { padding: 4px 0; border-bottom: 1px dotted #ccc; font-size: 11px; }
            .data-list li:last-child { border-bottom: none; }
            .score { color: #006600; font-weight: bold; }
            .back-link { display: block; text-align: center; margin: 20px 0; padding: 8px; 
                        background: #eee; border: 1px solid #999; text-decoration: none; 
                        color: #333; font-size: 11px; }
        ')],
        [div(class=container, [
            h1('Base de Faits - Vue Complète'),
            
            % Sources
            div(class=section, [
                h3(['Sources (', Sources, ' entrées)']),
                div(class='section-content', [
                    ul(class='data-list', 
                       [li([Source, ' - ', span(class=score, Score)]) || Source-Score <- Sources])
                ])
            ]),
            
            % Auteurs
            div(class=section, [
                h3(['Auteurs Reconnus (', Auteurs, ' entrées)']),
                div(class='section-content', [
                    ul(class='data-list', 
                       [li(Auteur) || Auteur <- Auteurs])
                ])
            ]),
            
            % Affirmations
            div(class=section, [
                h3(['Affirmations Fiables (', Affirmations, ' entrées)']),
                div(class='section-content', [
                    ul(class='data-list', 
                       [li([Org, ' → ', Sujet]) || Org-Sujet <- Affirmations])
                ])
            ]),
            
            % Mots Citation
            div(class=section, [
                h3(['Mots-clés Citation (', MotsCitation, ' entrées)']),
                div(class='section-content', [
                    ul(class='data-list', 
                       [li(Mot) || Mot <- MotsCitation])
                ])
            ]),
            
            % Mots Émotionnels
            div(class=section, [
                h3(['Mots-clés Émotionnels (', MotsEmotionnels, ' entrées)']),
                div(class='section-content', [
                    ul(class='data-list', 
                       [li(Mot) || Mot <- MotsEmotionnels])
                ])
            ]),
            
            % Mots Suspects
            div(class=section, [
                h3(['Mots-clés Suspects (', MotsSuspects, ' entrées)']),
                div(class='section-content', [
                    ul(class='data-list', 
                       [li(Mot) || Mot <- MotsSuspects])
                ])
            ]),
            
            % Mots Anonymes
            div(class=section, [
                h3(['Mots-clés Anonymes (', MotsAnonymes, ' entrées)']),
                div(class='section-content', [
                    ul(class='data-list', 
                       [li(Mot) || Mot <- MotsAnonymes])
                ])
            ]),
            
            a([href='/admin', class='back-link'], 'Retour Admin')
        ])]
    ).

% -------------------------------
% SUPPRESSION ADMIN
% -------------------------------
admin_delete(Request) :-
    http_parameters(Request, [delete_type(Type, [])]),
    process_admin_delete(Type, Message),
    
    reply_html_page(
        title('Suppression Effectuée'),
        [style('
            body { font-family: monospace; background: #f0f0f0; padding: 20px; }
            .container { max-width: 600px; margin: 0 auto; background: white; 
                        border: 1px solid #999; padding: 20px; text-align: center; }
            .warning { background: #ffeecc; border: 1px solid #cc6600; padding: 10px; margin: 15px 0; }
            a { display: inline-block; margin: 10px; padding: 8px 16px; background: #eee; 
                border: 1px solid #999; text-decoration: none; color: #333; }
        ')],
        [div(class=container, [
            h2('Suppression Effectuée'),
            div([class=warning], Message),
            a([href='/admin'], 'Retour Admin'),
            a([href='/'], 'Accueil')
        ])]
    ).

process_admin_delete(sources, 'Toutes les sources ont été supprimées') :-
    retractall(reputation(_, _)),
    % Rétablir les sources de base
    assert(reputation('inconnu', 0.1)),
    assert(reputation('anonyme', 0.1)),
    assert(reputation('', 0.1)).

process_admin_delete(auteurs, 'Tous les auteurs ont été supprimés') :-
    retractall(auteur_reconnu(_)).

process_admin_delete(affirmations, 'Toutes les affirmations ont été supprimées') :-
    retractall(affirmation_fiable(_, _)).

process_admin_delete(mots, 'Tous les mots-clés ont été supprimés') :-
    retractall(mot_citation(_)),
    retractall(mot_emotionnel(_)),
    retractall(mot_suspect(_)),
    retractall(mot_anonyme(_)),
    % Rétablir les mots anonymes de base
    assert(mot_anonyme('anonyme')),
    assert(mot_anonyme('inconnu')),
    assert(mot_anonyme('')).

process_admin_delete(all, 'Toute la base de données a été réinitialisée') :-
    retractall(reputation(_, _)),
    retractall(auteur_reconnu(_)),
    retractall(affirmation_fiable(_, _)),
    retractall(mot_citation(_)),
    retractall(mot_emotionnel(_)),
    retractall(mot_suspect(_)),
    retractall(mot_anonyme(_)),
    % Rétablir les données minimales
    assert(reputation('inconnu', 0.1)),
    assert(reputation('anonyme', 0.1)),
    assert(reputation('', 0.1)),
    assert(mot_anonyme('anonyme')),
    assert(mot_anonyme('inconnu')),
    assert(mot_anonyme('')).

% -------------------------------
% EXPORT DE LA BASE
% -------------------------------
admin_export(_Request) :-
    findall(reputation(Source, Score), reputation(Source, Score), RepFacts),
    findall(auteur_reconnu(Auteur), auteur_reconnu(Auteur), AutFacts),
    findall(affirmation_fiable(Org, Sujet), affirmation_fiable(Org, Sujet), AffFacts),
    findall(mot_citation(Mot), mot_citation(Mot), CitFacts),
    findall(mot_emotionnel(Mot), mot_emotionnel(Mot), EmoFacts),
    findall(mot_suspect(Mot), mot_suspect(Mot), SusFacts),
    findall(mot_anonyme(Mot), mot_anonyme(Mot), AnoFacts),
    
    append([RepFacts, AutFacts, AffFacts, CitFacts, EmoFacts, SusFacts, AnoFacts], AllFacts),
    
    format('Content-Type: text/plain~n'),
    format('Content-Disposition: attachment; filename="base_faits.pl"~n~n'),
    format('% Export de la base de faits - ~w~n~n', [date]),
    
    forall(member(Fact, AllFacts), (
        write_term(Fact, [quoted(true)]),
        write('.\n')
    )).

% -------------------------------
% PAGE DE RÉSULTATS (INCHANGÉE)
% -------------------------------
evaluate_article(Request) :-
    http_parameters(Request, [
        article(Article, []),
        source(Source, []),
        auteur(Auteur, [default('')])
    ]),
    evaluate_credibility(Article, Source, Auteur, FinalScore, Level, 
                        RepScore, AutScore, CitScore, StyleScore, CoherenceScore,
                        RepDetails, AutDetails, CitDetails, StyleDetails, CoherenceDetails),
    
    % Choix de la couleur selon le niveau
    (Level = 'TRÈS CRÉDIBLE' -> Color = '#006600' ;
     Level = 'CRÉDIBLE' -> Color = '#336600' ;
     Level = 'DOUTEUSE' -> Color = '#996600' ;
     Color = '#990000'),
    
    % Génération de la justification détaillée
    generate_justification(Level, RepScore, AutScore, CitScore, StyleScore, CoherenceScore, Justification),
    
    reply_html_page(
        title('Résultat Évaluation'),
        [style('
            body { 
                font-family: monospace; 
                background: #f8f8f8; 
                margin: 0; 
                padding: 20px;
                color: #333;
                line-height: 1.4;
            }
            .container { 
                max-width: 700px; 
                margin: 0 auto; 
                background: white; 
                border: 1px solid #ccc; 
                padding: 20px;
            }
            h1 { 
                text-align: center; 
                background: #333; 
                color: white; 
                padding: 8px; 
                margin: -20px -20px 20px -20px;
                font-size: 16px;
                font-weight: normal;
            }
            .result {
                border: 1px solid #999;
                padding: 10px;
                margin: 15px 0;
                text-align: center;
                font-size: 14px;
            }
            .criteria {
                border: 1px solid #999;
                width: 100%;
                border-collapse: collapse;
                margin: 15px 0;
                font-size: 11px;
            }
            .criteria th, .criteria td {
                border: 1px solid #999;
                padding: 6px;
                text-align: left;
            }
            .criteria th {
                background: #f0f0f0;
            }
            .justification {
                border: 1px solid #999;
                background: #f5f5f5;
                padding: 10px;
                margin: 15px 0;
                font-size: 11px;
            }
            .back {
                display: block;
                text-align: center;
                margin: 20px 0;
                padding: 8px;
                background: #eee;
                border: 1px solid #999;
                text-decoration: none;
                color: #333;
                font-size: 11px;
                text-transform: uppercase;
            }
            .back:hover { background: #ddd; }
        ')],
        [div(class=container, [
            h1('Résultat Évaluation'),
            
            div([class=result, style=['background: ', Color, '; color: white;']], [
                div(['NIVEAU: ', Level]), 
                div(['SCORE: ', FinalScore, '/85'])
            ]),
            
            table(class=criteria, [
                tr([
                    th('Critère'),
                    th('Score'),
                    th('Détails')
                ]),
                tr([
                    td('Réputation Source'),
                    td([RepScore, '/25']),
                    td(RepDetails)
                ]),
                tr([
                    td('Crédibilité Auteur'),
                    td([AutScore, '/25']),
                    td(AutDetails)
                ]),
                tr([
                    td('Citations/Références'),
                    td([CitScore, '/25']),
                    td(CitDetails)
                ]),
                tr([
                    td('Style Linguistique'),
                    td([StyleScore, '/15']),
                    td(StyleDetails)
                ]),
                tr([
                    td('Cohérence Factuelle'),
                    td([CoherenceScore, '/10']),
                    td(CoherenceDetails)
                ])
            ]),
            
            div(class=justification, [
                strong('Justification: '), 
                Justification
            ]),
            
            a([href='/', class=back], 'Nouvelle évaluation')
        ])]
    ).

% -------------------------------
% GÉNÉRATION DE JUSTIFICATION (INCHANGÉE)
% -------------------------------
generate_justification(Level, RepScore, AutScore, CitScore, StyleScore, CoherenceScore, Justification) :-
    findall(Raison, justification_reason(Level, RepScore, AutScore, CitScore, StyleScore, CoherenceScore, Raison), Raisons),
    (Raisons = [] -> 
        Justification = 'Évaluation basée sur les critères standards'
    ;
        atomic_list_concat(Raisons, ', ', Justification)
    ).

% Règles pour niveau SUSPECTE
justification_reason('SUSPECTE', RepScore, _, _, _, _, 'source non fiable') :-
    RepScore =< 5.
justification_reason('SUSPECTE', _, AutScore, _, _, _, 'auteur anonyme') :-
    AutScore =< 5.
justification_reason('SUSPECTE', _, _, CitScore, _, _, 'aucune référence') :-
    CitScore =< 5.
justification_reason('SUSPECTE', _, _, _, StyleScore, _, 'vocabulaire suspect') :-
    StyleScore =< 5.
justification_reason('SUSPECTE', _, _, _, _, CoherenceScore, 'incohérence factuelle') :-
    CoherenceScore =< 3.

% Règles pour niveau DOUTEUSE
justification_reason('DOUTEUSE', RepScore, _, _, _, _, 'source de réputation moyenne') :-
    RepScore > 5, RepScore < 20.
justification_reason('DOUTEUSE', _, AutScore, _, _, _, 'auteur non reconnu') :-
    AutScore > 5, AutScore < 20.
justification_reason('DOUTEUSE', _, _, CitScore, _, _, 'références partielles') :-
    CitScore > 5, CitScore < 20.
justification_reason('DOUTEUSE', _, _, _, StyleScore, _, 'style émotionnel') :-
    StyleScore > 5, StyleScore < 12.
justification_reason('DOUTEUSE', _, _, _, _, CoherenceScore, 'contradictions mineures') :-
    CoherenceScore > 3, CoherenceScore < 7.

% Règles pour niveau CRÉDIBLE
justification_reason('CRÉDIBLE', RepScore, _, _, _, _, 'source fiable') :-
    RepScore >= 20.
justification_reason('CRÉDIBLE', _, AutScore, _, _, _, 'auteur identifié') :-
    AutScore >= 15.
justification_reason('CRÉDIBLE', _, _, CitScore, _, _, 'références présentes') :-
    CitScore >= 15.
justification_reason('CRÉDIBLE', _, _, _, StyleScore, _, 'style approprié') :-
    StyleScore >= 12.
justification_reason('CRÉDIBLE', _, _, _, _, CoherenceScore, 'cohérence factuelle') :-
    CoherenceScore >= 7.

% Règles pour niveau TRÈS CRÉDIBLE
justification_reason('TRÈS CRÉDIBLE', RepScore, _, _, _, _, 'source très fiable') :-
    RepScore = 25.
justification_reason('TRÈS CRÉDIBLE', _, AutScore, _, _, _, 'auteur reconnu') :-
    AutScore = 25.
justification_reason('TRÈS CRÉDIBLE', _, _, CitScore, _, _, 'références solides') :-
    CitScore = 25.
justification_reason('TRÈS CRÉDIBLE', _, _, _, StyleScore, _, 'style neutre') :-
    StyleScore = 15.
justification_reason('TRÈS CRÉDIBLE', _, _, _, _, CoherenceScore, 'parfaite cohérence') :-
    CoherenceScore = 10.

% -------------------------------
% API JSON (INCHANGÉE)
% -------------------------------
api_evaluate(Request) :-
    http_read_json_dict(Request, Data),
    Article = Data.get(article),
    Source = Data.get(source),
    Auteur = Data.get(auteur, ''),
    evaluate_credibility(Article, Source, Auteur, FinalScore, Level, 
                        RepScore, AutScore, CitScore, StyleScore, CoherenceScore,
                        RepDetails, AutDetails, CitDetails, StyleDetails, CoherenceDetails),
    
    generate_justification(Level, RepScore, AutScore, CitScore, StyleScore, CoherenceScore, Justification),
    
    Reply = _{
        score: FinalScore, 
        level: Level,
        justification: Justification,
        criteria: _{
            reputation: _{score: RepScore, details: RepDetails},
            author: _{score: AutScore, details: AutDetails},
            citations: _{score: CitScore, details: CitDetails},
            style: _{score: StyleScore, details: StyleDetails},
            coherence: _{score: CoherenceScore, details: CoherenceDetails}
        }
    },
    reply_json_dict(Reply).

% -------------------------------
% COMMANDES DE GESTION ÉTENDUES
% -------------------------------
clear_dynamic_facts :-
    retractall(fait_extrait(_, _, _)),
    retractall(contradiction_detectee(_, _)),
    retractall(reference_croisee(_, _)).

show_extracted_facts :-
    findall(Fait-Article-Source, fait_extrait(Fait, Article, Source), Faits),
    format('Faits extraits: ~w~n', [Faits]).

% Commandes pour gestion manuelle de la base
add_source(Name, Score) :-
    downcase_atom(Name, NameLower),
    (reputation(NameLower, _) ->
        retract(reputation(NameLower, _))
    ; true),
    assert(reputation(NameLower, Score)),
    format('Source ~w ajoutée avec score ~2f~n', [Name, Score]).

add_author(Name) :-
    (auteur_reconnu(Name) ->
        format('Auteur ~w déjà existant~n', [Name])
    ; assert(auteur_reconnu(Name)),
      format('Auteur ~w ajouté~n', [Name])
    ).

add_reliable_claim(Org, Subject) :-
    (affirmation_fiable(Org, Subject) ->
        format('Affirmation ~w -> ~w déjà existante~n', [Org, Subject])
    ; assert(affirmation_fiable(Org, Subject)),
      format('Affirmation ~w -> ~w ajoutée~n', [Org, Subject])
    ).

add_keyword(Type, Word) :-
    downcase_atom(Word, WordLower),
    add_keyword_by_type(Type, WordLower).

add_keyword_by_type(citation, Word) :-
    (mot_citation(Word) ->
        format('Mot-clé citation ~w déjà existant~n', [Word])
    ; assert(mot_citation(Word)),
      format('Mot-clé citation ~w ajouté~n', [Word])
    ).

add_keyword_by_type(emotional, Word) :-
    (mot_emotionnel(Word) ->
        format('Mot-clé émotionnel ~w déjà existant~n', [Word])
    ; assert(mot_emotionnel(Word)),
      format('Mot-clé émotionnel ~w ajouté~n', [Word])
    ).

add_keyword_by_type(suspect, Word) :-
    (mot_suspect(Word) ->
        format('Mot-clé suspect ~w déjà existant~n', [Word])
    ; assert(mot_suspect(Word)),
      format('Mot-clé suspect ~w ajouté~n', [Word])
    ).

add_keyword_by_type(anonymous, Word) :-
    (mot_anonyme(Word) ->
        format('Mot-clé anonyme ~w déjà existant~n', [Word])
    ; assert(mot_anonyme(Word)),
      format('Mot-clé anonyme ~w ajouté~n', [Word])
    ).

% Statistiques de la base
show_database_stats :-
    findall(_, reputation(_, _), Sources),
    findall(_, auteur_reconnu(_), Auteurs),
    findall(_, affirmation_fiable(_, _), Affirmations),
    findall(_, mot_citation(_), Citations),
    findall(_, mot_emotionnel(_), Emotionnels),
    findall(_, mot_suspect(_), Suspects),
    findall(_, mot_anonyme(_), Anonymes),
    
    length(Sources, NbSources),
    length(Auteurs, NbAuteurs),
    length(Affirmations, NbAffirmations),
    length(Citations, NbCitations),
    length(Emotionnels, NbEmotionnels),
    length(Suspects, NbSuspects),
    length(Anonymes, NbAnonymes),
    
    format('=== STATISTIQUES DE LA BASE ===~n'),
    format('Sources: ~w~n', [NbSources]),
    format('Auteurs reconnus: ~w~n', [NbAuteurs]),
    format('Affirmations fiables: ~w~n', [NbAffirmations]),
    format('Mots-clés citation: ~w~n', [NbCitations]),
    format('Mots-clés émotionnels: ~w~n', [NbEmotionnels]),
    format('Mots-clés suspects: ~w~n', [NbSuspects]),
    format('Mots-clés anonymes: ~w~n', [NbAnonymes]),
    Total is NbSources + NbAuteurs + NbAffirmations + NbCitations + NbEmotionnels + NbSuspects + NbAnonymes,
    format('TOTAL: ~w entrées~n', [Total]).

% Sauvegarde de la base dans un fichier
save_database(Filename) :-
    open(Filename, write, Stream),
    format(Stream, '% Base de faits sauvegardée automatiquement~n~n', []),
    
    % Sources
    forall(reputation(Source, Score), 
           format(Stream, 'reputation(~q, ~w).~n', [Source, Score])),
    
    % Auteurs
    forall(auteur_reconnu(Auteur), 
           format(Stream, 'auteur_reconnu(~q).~n', [Auteur])),
    
    % Affirmations
    forall(affirmation_fiable(Org, Sujet), 
           format(Stream, 'affirmation_fiable(~q, ~q).~n', [Org, Sujet])),
    
    % Mots-clés
    forall(mot_citation(Mot), 
           format(Stream, 'mot_citation(~q).~n', [Mot])),
    forall(mot_emotionnel(Mot), 
           format(Stream, 'mot_emotionnel(~q).~n', [Mot])),
    forall(mot_suspect(Mot), 
           format(Stream, 'mot_suspect(~q).~n', [Mot])),
    forall(mot_anonyme(Mot), 
           format(Stream, 'mot_anonyme(~q).~n', [Mot])),
    
    close(Stream),
    format('Base sauvegardée dans ~w~n', [Filename]).

% Pour démarrer: ?- start_server(8080).
% Interface admin accessible sur: http://localhost:8080/admin