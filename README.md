# 🌍 Convergence économique entre les pays du RCEP (1950–2018)

Ce projet analyse l'existence d'une convergence économique entre les pays membres du Partenariat Économique Régional Global (RCEP), à partir de l'évolution du PIB par habitant sur la période 1950–2018. Il s'agit d'un travail académique réalisé dans le cadre du Master 1 Économie Appliquée à l'Université de Pau et des Pays de l’Adour.

---

## 🎯 Objectifs

- Étudier les écarts de développement économique entre les pays de la zone RCEP
- Vérifier empiriquement l’existence de **β-convergence** et **σ-convergence**
- Analyser la dynamique de transition entre groupes de pays par des **matrices de mobilité**
- Identifier les clubs de convergence et les trajectoires économiques à long terme

---

## 🛠 Outils et Méthodes

- Langage **R**
- Packages utilisés : `ggplot2`, `dplyr`, `plm`, `tidyverse`, `mFilter`
- Estimations :
  - Régressions pour la β-convergence (absolute et conditionnelle)
  - Analyse de σ-convergence via l'écart-type du PIB par habitant
  - Tests de nombre de modes avec **Silverman's Test**
  - Matrices de transition à 5 et 10 ans entre groupes de pays (clubs)

---

## 📈 Résultats

- En 1950, deux groupes de pays étaient clairement identifiables (riches vs pauvres)
- À partir de 1990, une tendance à la **convergence économique** se dessine, confirmée par :
  - La diminution de la **dispersion** (σ)
  - Des régressions de croissance négatives (β)
- Les pays pauvres tendent à **rattraper progressivement** les pays les plus riches, malgré la persistance de certains écarts
- Les **matrices de mobilité** révèlent une forte inertie : les pays riches restent riches, mais certains pays pauvres changent de groupe

---

## 📁 Contenu du dépôt
```
convergence_economique/
├── convergence_rcep.R # Script principal en R
├── rapport_convergence.pdf # Rapport complet (optionnel)
└── README.md # Présentation du projet
```

## 🧠 Auteurs

Projet réalisé par **Youcef Belabbas**  
🔗 [LinkedIn](https://www.linkedin.com/in/youcef-belabbas-83a86a259/)  
📩 youcefbelabbas97@gmail.com
