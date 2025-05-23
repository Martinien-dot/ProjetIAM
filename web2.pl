% ===============================================
% SERVEUR WEB PROLOG - EVALUATEUR DE FIABILITE
% ===============================================

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
% BASE DE CONNAISSANCES
% -------------------------------
reputation('Le Monde', 0.9).
reputation('Infowars', 0.1).
reputation('Science Today', 0.85).
reputation('Random Blog', 0.3).
reputation('National Geographic', 0.95).
reputation('Facebook Post', 0.2).
reputation('BBC', 0.88).
reputation('Reuters', 0.92).

affirmation_fiable('GIEC', 'réchauffement climatique').
affirmation_fiable('OMS', 'COVID-19').
affirmation_fiable('ONU', 'plastique').
affirmation_fiable('NASA', 'pyramides').

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

% -------------------------------
% CRITÈRES D'ÉVALUATION DÉTAILLÉS
% -------------------------------

% Critère 1: Réputation de la source (40 points)
evaluate_reputation(Source, Score, Details) :-
    (reputation(Source, Rep) ->
        (Rep >= 0.8 -> 
            Score = 40, Details = 'Source très fiable (réputation >= 0.8)'
        ; Rep >= 0.5 -> 
            Score = 30, Details = 'Source moyennement fiable (réputation >= 0.5)'
        ; 
            Score = 0, format(atom(Details), 'Source peu fiable (réputation: ~2f)', [Rep])
        )
    ; 
        Score = 0, Details = 'Source inconnue dans notre base'
    ).

% Critère 2: Présence de citations (20 points)
evaluate_citations(Article, Score, Details) :-
    % Détection par mots-clés simples
    findall(Mot, (mot_citation(Mot), sub_string(Article, _, _, _, Mot)), MotsCles),
    length(MotsCles, NbMotsCles),
    
    % Détection simple de citations avec guillemets
    (re_match('"[^"]+"', Article) -> 
        HasCitation = 1, CitationText = 'Citation détectée'
    ; 
        HasCitation = 0, CitationText = 'Aucune citation'
    ),
    
    % Score combiné
    TotalScore is NbMotsCles + HasCitation,
    
    (TotalScore >= 3 ->
        Score = 20, format(atom(Details), 'Références solides: ~w mots-clés + ~w', [NbMotsCles, CitationText])
    ; TotalScore >= 1 ->
        Score = 15, format(atom(Details), 'Quelques références: ~w mots-clés + ~w', [NbMotsCles, CitationText])
    ;
        Score = 0, Details = 'Aucune référence ou citation détectée'
    ).

% Critère 3: Style de langage (20 points)
evaluate_style(Article, Score, Details) :-
    findall(Mot, (mot_emotionnel(Mot), sub_string(Article, _, _, _, Mot)), MotsEmotionnels),
    findall(Mot, (mot_suspect(Mot), sub_string(Article, _, _, _, Mot)), MotsSuspects),
    length(MotsEmotionnels, NbEmotionnels),
    length(MotsSuspects, NbSuspects),
    
    (NbEmotionnels = 0, NbSuspects = 0 ->
        Score = 20, Details = 'Langage neutre et objectif'
    ; NbEmotionnels > 0, NbSuspects = 0 ->
        Score = 10, format(atom(Details), 'Langage émotionnel détecté (~w mots)', [NbEmotionnels])
    ; NbSuspects > 0 ->
        Score = 0, format(atom(Details), 'Vocabulaire suspect détecté (~w mots douteux)', [NbSuspects])
    ;
        Score = 5, Details = 'Style mixte détecté'
    ).

% Critère 4: Cohérence avec sources fiables (20 points)
evaluate_coherence(Article, Score, Details) :-
    findall(Org-Sujet, (
        affirmation_fiable(Org, Sujet),
        sub_string(Article, _, _, _, Sujet),
        (sub_string(Article, _, _, _, 'mensonge') ; 
         sub_string(Article, _, _, _, 'faux') ;
         sub_string(Article, _, _, _, 'démenti'))
    ), Contradictions),
    
    (Contradictions = [] ->
        Score = 20, Details = 'Aucune contradiction avec les sources fiables'
    ;
        Score = 0,
        length(Contradictions, NbContradictions),
        format(atom(Details), 'Contredit ~w source(s) fiable(s)', [NbContradictions])
    ).

% -------------------------------
% ÉVALUATION GLOBALE
% -------------------------------
evaluate_credibility(Article, Source, FinalScore, Level, RepScore, CitScore, StyleScore, CoherScore, RepDetails, CitDetails, StyleDetails, CoherDetails) :-
    evaluate_reputation(Source, RepScore, RepDetails),
    evaluate_citations(Article, CitScore, CitDetails),
    evaluate_style(Article, StyleScore, StyleDetails),
    evaluate_coherence(Article, CoherScore, CoherDetails),
    
    FinalScore is RepScore + CitScore + StyleScore + CoherScore,
    
    % Classification
    (FinalScore >= 70 -> Level = 'TRÈS CRÉDIBLE' ;
     FinalScore >= 50 -> Level = 'CRÉDIBLE' ;
     FinalScore >= 30 -> Level = 'DOUTEUSE' ;
     Level = 'SUSPECTE').

% -------------------------------
% INTERFACE WEB STYLE ANNÉES 2000
% -------------------------------
home_page(_Request) :-
    reply_html_page(
        title('*** EVALUATEUR DE FIABILITE v1.0 ***'),
        [style('
            body { 
                font-family: "Courier New", monospace; 
                background: #f0f0f0; 
                margin: 0; 
                padding: 20px;
                color: #333;
            }
            .container { 
                max-width: 700px; 
                margin: 0 auto; 
                background: white; 
                border: 2px solid #000; 
                padding: 20px;
            }
            h1 { 
                text-align: center; 
                background: #000; 
                color: #00ff00; 
                padding: 10px; 
                margin: -20px -20px 20px -20px;
                font-size: 18px;
            }
            .form-section { 
                border: 1px solid #999; 
                padding: 15px; 
                margin: 15px 0; 
                background: #fafafa;
            }
            label { 
                display: block; 
                font-weight: bold; 
                margin-bottom: 5px; 
                font-size: 14px;
            }
            input, textarea { 
                width: 95%; 
                padding: 5px; 
                border: 1px solid #666; 
                font-family: "Courier New", monospace;
                font-size: 12px;
            }
            button { 
                background: #ddd; 
                border: 2px outset #ddd; 
                padding: 8px 16px; 
                font-family: "Courier New", monospace;
                font-weight: bold;
                cursor: pointer;
                margin: 10px 0;
            }
            button:hover { background: #ccc; }
            .info-box {
                border: 1px solid #000;
                background: #ffffcc;
                padding: 10px;
                margin: 10px 0;
                font-size: 12px;
            }
            .footer {
                text-align: center;
                font-size: 10px;
                color: #666;
                margin-top: 20px;
            }
        ')],
        [div(class=container, [
            h1('*** EVALUATEUR DE FIABILITE v1.0 ***'),
            
            form([action=evaluate, method=post], [
                div(class='form-section', [
                    label(for=article, 'CONTENU DE L\'ARTICLE:'),
                    textarea([name=article, id=article, rows=8, required, 
                             placeholder='Collez ici le texte complet de l\'article...'], '')
                ]),
                
                div(class='form-section', [
                    label(for=source, 'SOURCE:'),
                    input([name=source, id=source, type=text, required, 
                          placeholder='Ex: Le Monde, BBC, Facebook Post'], [])
                ]),
                
                div(style='text-align: center;', [
                    button([type=submit], '[EVALUER LA FIABILITE]')
                ])
            ]),
            
            div(class='info-box', [
                strong('SOURCES CONNUES:'), br,
                'Le Monde (0.9) | BBC (0.88) | Reuters (0.92) | National Geographic (0.95)', br,
                'Science Today (0.85) | Random Blog (0.3) | Infowars (0.1) | Facebook Post (0.2)'
            ]),
            
            div(class='info-box', [
                strong('CRITÈRES D\'ÉVALUATION:'), br,
                '1. Réputation source (40 pts) | 2. Citations + Guillemets (20 pts)', br,
                '3. Style langage (20 pts) | 4. Cohérence sources fiables (20 pts)'
            ]),
            
            div(class=footer, [
                '--- Système Expert de Fiabilité --- Powered by SWI-Prolog ---'
            ])
        ])]
    ).

% -------------------------------
% PAGE DE RÉSULTATS DÉTAILLÉE
% -------------------------------
evaluate_article(Request) :-
    http_parameters(Request, [
        article(Article, []),
        source(Source, [])
    ]),
    evaluate_credibility(Article, Source, FinalScore, Level, 
                        RepScore, CitScore, StyleScore, CoherScore,
                        RepDetails, CitDetails, StyleDetails, CoherDetails),
    
    % Choix de la couleur selon le niveau
    (Level = 'TRÈS CRÉDIBLE' -> Color = '#00aa00' ;
     Level = 'CRÉDIBLE' -> Color = '#66aa00' ;
     Level = 'DOUTEUSE' -> Color = '#aa6600' ;
     Color = '#aa0000'),
    
    reply_html_page(
        title('*** RESULTAT EVALUATION ***'),
        [style('
            body { 
                font-family: "Courier New", monospace; 
                background: #f0f0f0; 
                margin: 0; 
                padding: 20px;
                color: #333;
            }
            .container { 
                max-width: 800px; 
                margin: 0 auto; 
                background: white; 
                border: 2px solid #000; 
                padding: 20px;
            }
            h1 { 
                text-align: center; 
                background: #000; 
                color: #00ff00; 
                padding: 10px; 
                margin: -20px -20px 20px -20px;
                font-size: 18px;
            }
            .result-box {
                border: 2px solid #000;
                padding: 15px;
                margin: 15px 0;
                text-align: center;
                font-size: 16px;
                font-weight: bold;
            }
            .criteria-table {
                border: 1px solid #000;
                width: 100%;
                border-collapse: collapse;
                margin: 15px 0;
                font-size: 12px;
            }
            .criteria-table th, .criteria-table td {
                border: 1px solid #000;
                padding: 8px;
                text-align: left;
            }
            .criteria-table th {
                background: #ddd;
                font-weight: bold;
            }
            .score-good { color: #006600; font-weight: bold; }
            .score-medium { color: #996600; font-weight: bold; }
            .score-bad { color: #cc0000; font-weight: bold; }
            .back-link {
                display: block;
                text-align: center;
                margin: 20px 0;
                padding: 10px;
                background: #eee;
                border: 1px solid #999;
                text-decoration: none;
                color: #000;
            }
            .back-link:hover { background: #ddd; }
        ')],
        [div(class=container, [
            h1('*** RESULTAT EVALUATION ***'),
            
            div([class='result-box', style=['background-color: ', Color, '; color: white;']], [
                h2(['NIVEAU: ', Level]), 
                h3(['SCORE GLOBAL: ', FinalScore, '/100'])
            ]),
            
            h3('DÉTAIL PAR CRITÈRE:'),
            table(class='criteria-table', [
                tr([
                    th('CRITÈRE'),
                    th('SCORE'),
                    th('DÉTAILS')
                ]),
                tr([
                    td('1. Réputation Source (40 pts max)'),
                    td([class=score_class(RepScore, 40), RepScore, '/40']),
                    td(RepDetails)
                ]),
                tr([
                    td('2. Citations/Références (20 pts max)'),
                    td([class=score_class(CitScore, 20), CitScore, '/20']),
                    td(CitDetails)
                ]),
                tr([
                    td('3. Style de Langage (20 pts max)'),
                    td([class=score_class(StyleScore, 20), StyleScore, '/20']),
                    td(StyleDetails)
                ]),
                tr([
                    td('4. Cohérence Sources (20 pts max)'),
                    td([class=score_class(CoherScore, 20), CoherScore, '/20']),
                    td(CoherDetails)
                ])
            ]),
            
            div(class='info-box', [
                strong('BARÊME:'), br,
                '70-100: TRÈS CRÉDIBLE | 50-69: CRÉDIBLE | 30-49: DOUTEUSE | 0-29: SUSPECTE'
            ]),
            
            a([href='/', class='back-link'], '[RETOUR - NOUVELLE EVALUATION]')
        ])]
    ).

% Helper pour déterminer la classe CSS du score
score_class(Score, MaxScore, Class) :-
    Percentage is (Score / MaxScore) * 100,
    (Percentage >= 75 -> Class = 'score-good' ;
     Percentage >= 50 -> Class = 'score-medium' ;
     Class = 'score-bad').

% -------------------------------
% API JSON
% -------------------------------
api_evaluate(Request) :-
    http_read_json_dict(Request, Data),
    Article = Data.get(article),
    Source = Data.get(source),
    evaluate_credibility(Article, Source, FinalScore, Level, 
                        RepScore, CitScore, StyleScore, CoherScore,
                        RepDetails, CitDetails, StyleDetails, CoherDetails),
    
    Reply = _{
        score: FinalScore, 
        level: Level,
        criteria: _{
            reputation: _{score: RepScore, details: RepDetails},
            citations: _{score: CitScore, details: CitDetails},
            style: _{score: StyleScore, details: StyleDetails},
            coherence: _{score: CoherScore, details: CoherDetails}
        }
    },
    reply_json_dict(Reply).

% -------------------------------
% COMMANDES DE DEMARRAGE
% -------------------------------
% Pour démarrer le serveur, utilisez :
% ?- start_server(8080).