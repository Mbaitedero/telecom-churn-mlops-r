# Telecom Churn MLOps (R)

[![CI](https://github.com/Mbaitedero/telecom-churn-mlops-r/actions/workflows/ci.yml/badge.svg)](https://github.com/Mbaitedero/telecom-churn-mlops-r/actions/workflows/ci.yml)

Pipeline MLOps complet de prédiction du **churn client** d'un opérateur télécom,
entièrement en R : validation des données, prétraitement, entraînement,
évaluation, API REST, monitoring de dérive, tests, Docker, DVC et CI.

Données : [Telco Customer Churn](https://www.kaggle.com/datasets/blastchar/telco-customer-churn)
(7 043 clients, 21 colonnes, environ 26,5 % de churn).

## Résultats

Découpage stratifié 70 / 15 / 15 (train / validation / test). Le jeu test n'est
utilisé qu'une seule fois, pour l'évaluation finale.

| Modèle (validation) | AUC | Recall | Precision | F1 |
|---|---|---|---|---|
| Régression logistique | 0,847 | 0,593 | 0,669 | 0,629 |
| Random Forest | 0,838 | 0,496 | 0,675 | 0,572 |
| XGBoost | 0,837 | 0,511 | 0,636 | 0,566 |

**Modèle final** : régression logistique régularisée (`glmnet`), sans SMOTE,
seuil de décision à **0,30** choisi par validation croisée sur le jeu
d'entraînement.

| Jeu test | Accuracy | Recall | Precision | F1 | AUC |
|---|---|---|---|---|---|
| Seuil 0,30 | 0,762 | 0,769 | 0,536 | 0,632 | 0,842 |

Choix de conception :

- La régression logistique est au moins aussi bonne que Random Forest et XGBoost
  sur ce jeu de données, tout en restant interprétable.
- Régler les hyperparamètres et ajouter SMOTE n'améliorent pas l'AUC : SMOTE
  revient à déplacer le seuil, et déforme les probabilités. Seul le seuil est
  donc ajusté.
- Abaisser le seuil de 0,50 à 0,30 fait passer le recall de 0,59 à 0,77 : le
  modèle détecte beaucoup plus de clients qui partent, au prix de plus de faux
  positifs. C'est un choix métier (une offre de rétention coûte moins cher
  qu'un client perdu).

## Architecture

```
telecom-churn-mlops-r/
├── R/                  # fonctions du projet
│   ├── data_ingestion.R    # lecture, découpage train/valid/test
│   ├── data_validation.R   # contrôles qualité et EDA
│   ├── preprocessing.R     # nettoyage, recipe (numérique + catégoriel)
│   ├── train.R             # modèles, réglage glmnet, seuil
│   ├── evaluate.R          # évaluation finale sur le jeu test
│   ├── predict.R           # chargement du modèle et prédiction
│   └── monitoring.R        # dérive des données (PSI, KS) et performance
├── plumber/api.R       # API REST (/health, /predict)
├── scripts/            # points d'entrée manuels
├── tests/testthat/     # tests unitaires
├── _targets.R          # pipeline reproductible (targets)
├── dvc.yaml            # versionnement des données et du pipeline
├── Dockerfile          # image de l'API
├── docker-compose.yml
├── renv.lock           # environnement R figé
└── .github/workflows/ci.yml
```

## Démarrage

Prérequis : R 4.5.2, [DVC](https://dvc.org), Docker (facultatif).

```bash
git clone https://github.com/Mbaitedero/telecom-churn-mlops-r.git
cd telecom-churn-mlops-r
```

Place le fichier du jeu de données dans `data/raw/churn.csv`, puis, dans R :

```r
renv::restore()          # installe les packages aux versions figées
targets::tar_make()      # lance tout le pipeline
```

Résultats : `models/final_model.rds`, `reports/metrics/`, `reports/figures/`.

Avec DVC : `dvc repro`.

## API

```bash
Rscript -e "plumber::plumb(file = 'plumber/api.R')\$run(port = 8000)"
```

```bash
curl -s http://127.0.0.1:8000/health
# {"status":"ok","model_loaded":true,"threshold":0.3}

curl -s -X POST http://127.0.0.1:8000/predict \
  -H "Content-Type: application/json" \
  -d @tests/sample_customer.json
# {"threshold":0.3,"n":1,"predictions":[{"churn_probability":0.6891,"prediction":"Yes"}]}
```

Un JSON invalide ou incomplet renvoie une erreur 400 qui liste les colonnes
manquantes.

## Docker

Le modèle doit exister avant la construction (`targets::tar_make()`).

```bash
docker compose up --build
```

## Tests

```r
testthat::test_dir("tests/testthat")
```

Les tests couvrent le nettoyage, le découpage, la validation, la prédiction et
le monitoring. Ils tournent automatiquement sur GitHub Actions.

## Monitoring

`R/monitoring.R` compare les nouvelles données à la référence (jeu
d'entraînement) :

- **PSI** par variable et sur le score du modèle (< 0,10 stable,
  0,10 à 0,25 à surveiller, ≥ 0,25 alerte) ;
- **test de Kolmogorov-Smirnov** pour les variables numériques ;
- **performance** (recall, precision, AUC) quand les vrais résultats sont
  connus.

`scripts/monitor_model.R` en montre un exemple avec une dérive simulée.

## Limites et pistes


- Jeu de données public et propre : en production, il faudrait journaliser les
  requêtes de l'API pour alimenter le monitoring.
- Le seuil (0,30) dépend du coût réel d'une offre de rétention par rapport à
  celui d'un client perdu.
- DVC n'a pas de stockage distant configuré : le jeu de données se place à la
  main dans `data/raw/`.
