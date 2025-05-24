% ===============================================
% SERVEUR WEB PROLOG - EVALUATEUR DE FIABILITE v2.1
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

% Démarrage du serveur
start_server(Port) :-
    http_server(http_dispatch, [port(Port)]),
    format('Serveur démarré sur http://localhost:~w~n', [Port]).

% -------------------------------
% BASE DE CONNAISSANCES STATIQUE
% -------------------------------
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
reputation('the washington post', 0.88).
reputation('the new york times', 0.89).
reputation('the economist', 0.86).
reputation('scientific american', 0.87).
reputation('nature journal', 0.93).
reputation('science magazine', 0.92).
reputation('the lancet', 0.94).
reputation('forbes', 0.78).
reputation('bloomberg', 0.82).
reputation('time magazine', 0.81).
reputation('the wall street journal', 0.85).
reputation('ap news', 0.91).
reputation('npr', 0.87).
reputation('pbs', 0.86).
reputation('smithsonian magazine', 0.84).
reputation('national review', 0.72).
reputation('the atlantic', 0.83).
reputation('wired', 0.79).
reputation('techcrunch', 0.77).
reputation('the verge', 0.76).
reputation('ars technica', 0.81).
reputation('popular science', 0.75).
reputation('new scientist', 0.84).
reputation('science daily', 0.82).
reputation('mit technology review', 0.85).
reputation('harvard business review', 0.86).
reputation('the conversation', 0.83).
reputation('propublica', 0.87).
reputation('the intercept', 0.73).
reputation('vox', 0.74).

affirmation_fiable('GIEC', 'réchauffement climatique').
affirmation_fiable('OMS', 'COVID-19').
affirmation_fiable('ONU', 'plastique').
affirmation_fiable('NASA', 'pyramides').
affirmation_fiable('NASA', 'exploration spatiale').
affirmation_fiable('NASA', 'climat terrestre').
affirmation_fiable('NASA', 'astrophysique').
affirmation_fiable('WHO', 'santé mondiale').
affirmation_fiable('CDC', 'maladies infectieuses').
affirmation_fiable('FDA', 'médicaments').
affirmation_fiable('EPA', 'environnement').
affirmation_fiable('IMF', 'économie mondiale').
affirmation_fiable('World Bank', 'développement').
affirmation_fiable('IEEE', 'technologie').
affirmation_fiable('ACM', 'informatique').
affirmation_fiable('AAAS', 'science').
affirmation_fiable('Royal Society', 'recherche').
affirmation_fiable('Max Planck Society', 'physique').
affirmation_fiable('CERN', 'physique des particules').
affirmation_fiable('ESA', 'espace').
affirmation_fiable('JAXA', 'astronautique').
affirmation_fiable('ISRO', 'programme spatial').
affirmation_fiable('NSF', 'financement scientifique').
affirmation_fiable('NIH', 'recherche médicale').
affirmation_fiable('UNESCO', 'éducation').
affirmation_fiable('FAO', 'agriculture').
affirmation_fiable('ILO', 'travail').
affirmation_fiable('ITU', 'télécommunications').
affirmation_fiable('WMO', 'météorologie').
affirmation_fiable('IAEA', 'énergie atomique').
affirmation_fiable('OECD', 'statistiques').
affirmation_fiable('WTO', 'commerce').
affirmation_fiable('ICRC', 'droit humanitaire').
affirmation_fiable('Amnesty International', 'droits humains').
affirmation_fiable('Human Rights Watch', 'violations').
affirmation_fiable('Transparency International', 'corruption').
affirmation_fiable('Pew Research Center', 'sondages').

% Auteurs reconnus
auteur_reconnu('Jean Dupont').
auteur_reconnu('Dr. Martin').
auteur_reconnu('Prof. Durand').
auteur_reconnu('Stephen Hawking').
auteur_reconnu('Neil deGrasse Tyson').
auteur_reconnu('Michio Kaku').
auteur_reconnu('Jane Goodall').
auteur_reconnu('Richard Dawkins').
auteur_reconnu('Steven Pinker').
auteur_reconnu('Yuval Noah Harari').
auteur_reconnu('Malcolm Gladwell').
auteur_reconnu('Atul Gawande').
auteur_reconnu('Oliver Sacks').
auteur_reconnu('Brian Greene').
auteur_reconnu('Lisa Randall').
auteur_reconnu('Carl Sagan').
auteur_reconnu('Rachel Carson').
auteur_reconnu('E.O. Wilson').
auteur_reconnu('Jared Diamond').
auteur_reconnu('Daniel Kahneman').
auteur_reconnu('Angela Duckworth').
auteur_reconnu('Mary Roach').
auteur_reconnu('Bill Bryson').
auteur_reconnu('David Attenborough').
auteur_reconnu('Sylvia Earle').
auteur_reconnu('Paul Krugman').
auteur_reconnu('Joseph Stiglitz').
auteur_reconnu('Thomas Friedman').
auteur_reconnu('Fareed Zakaria').
auteur_reconnu('Christiane Amanpour').
auteur_reconnu('Anderson Cooper').
auteur_reconnu('David Brooks').
auteur_reconnu('Maureen Dowd').

% Mots-clés pour détection
mot_citation('étude').
mot_citation('recherche').
mot_citation('selon').
mot_citation('données').
mot_citation('rapport').
mot_citation('analyse').
mot_citation('méta-analyse').
mot_citation('revue par les pairs').
mot_citation('essai clinique').
mot_citation('étude longitudinale').
mot_citation('échantillon représentatif').
mot_citation('intervalle de confiance').
mot_citation('significativité statistique').
mot_citation('méthodologie').
mot_citation('bibliographie').
mot_citation('citation').
mot_citation('référence').
mot_citation('source primaire').
mot_citation('source secondaire').
mot_citation('littérature académique').
mot_citation('journal scientifique').
mot_citation('conférence').
mot_citation('symposium').
mot_citation('colloque').
mot_citation('publication universitaire').
mot_citation('thèse doctorale').
mot_citation('enquête').
mot_citation('sondage').
mot_citation('données empiriques').
mot_citation('preuves tangibles').
mot_citation('corpus documentaire').
mot_citation('archives').
mot_citation('recension').
mot_citation('examen systématique').
mot_citation('validation expérimentale').
mot_citation('protocole de recherche').

mot_emotionnel('incroyable').
mot_emotionnel('choquant').
mot_emotionnel('scandaleux').
mot_emotionnel('urgent').
mot_emotionnel('alerte').
mot_emotionnel('révélation').
mot_emotionnel('effroyable').
mot_emotionnel('terrifiant').
mot_emotionnel('catastrophique').
mot_emotionnel('miraculeux').
mot_emotionnel('prodigieux').
mot_emotionnel('hallucinant').
mot_emotionnel('stupéfiant').
mot_emotionnel('époustouflant').
mot_emotionnel('révolutionnaire').
mot_emotionnel('apocalyptique').
mot_emotionnel('désastreux').
mot_emotionnel('merveilleux').
mot_emotionnel('fantastique').
mot_emotionnel('horripilant').
mot_emotionnel('exaspérant').
mot_emotionnel('consternant').
mot_emotionnel('édifiant').
mot_emotionnel('saisissant').
mot_emotionnel('bouleversant').
mot_emotionnel('ahurissant').
mot_emotionnel('renversant').
mot_emotionnel('sidérant').
mot_emotionnel('tragique').
mot_emotionnel('dramatique').
mot_emotionnel('spectaculaire').
mot_emotionnel('impensable').
mot_emotionnel('inimaginable').
mot_emotionnel('inqualifiable').
mot_emotionnel('inouï').
mot_emotionnel('incroyable mais vrai').

mot_suspect('mensonge').
mot_suspect('faux').
mot_suspect('démenti').
mot_suspect('complot').
mot_suspect('secret').
mot_suspect('caché').
mot_suspect('élite secrète').
mot_suspect('agenda caché').
mot_suspect('vérité interdite').
mot_suspect('officiels menteurs').
mot_suspect('science corrompue').
mot_suspect('médias vendus').
mot_suspect('dissimulation').
mot_suspect('manipulation massive').
mot_suspect('propagande').
mot_suspect('endoctrinement').
mot_suspect('nous cachent tout').
mot_suspect('on nous ment').
mot_suspect('révélation choc').
mot_suspect('vérité censurée').
mot_suspect('théorie officielle').
mot_suspect('version mensongère').
mot_suspect('faits alternatifs').
mot_suspect('preuves ignorées').
mot_suspect('témoins réduits au silence').
mot_suspect('scientifiques achetés').
mot_suspect('journalistes complices').
mot_suspect('gouvernement criminel').
mot_suspect('expérience interdite').
mot_suspect('technologie secrète').
mot_suspect('découverte supprimée').
mot_suspect('histoire réécrite').
mot_suspect('vérité alternative').
mot_suspect('preuves accablantes').
mot_suspect('scandale étouffé').
mot_suspect('conspiration').

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
% CRITÈRES D'ÉVALUATION CORRIGÉS
% -------------------------------

% Critère 1: Réputation de la source (25 points) - CORRIGÉ
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

% Critère 2: Évaluation de l'auteur (25 points) - CORRIGÉ
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

% Critère 3: Présence de citations et références (25 points) - CORRIGÉ
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
% ÉVALUATION GLOBALE CORRIGÉE (5 critères)
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
% INTERFACE WEB SOBRE
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
        ')],
        [div(class=container, [
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
        ])]
    ).

% -------------------------------
% PAGE DE RÉSULTATS SOBRE - CORRIGÉE
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
% GÉNÉRATION DE JUSTIFICATION AMÉLIORÉE
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
% API JSON SIMPLIFIÉE
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
% COMMANDES DE GESTION
% -------------------------------
clear_dynamic_facts :-
    retractall(fait_extrait(_, _, _)),
    retractall(contradiction_detectee(_, _)),
    retractall(reference_croisee(_, _)).

show_extracted_facts :-
    findall(Fait-Article-Source, fait_extrait(Fait, Article, Source), Faits),
    format('Faits extraits: ~w~n', [Faits]).

% Pour démarrer: ?- start_server(8080).