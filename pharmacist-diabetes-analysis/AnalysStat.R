#packages import
library(skimr)
library(tableone)
library(stats)   
library(ggplot2)
#avant anlysise, description de l'echantillon

skim(patiens_df)
'''
 p0	40 → le plus jeune patient a 40 ans
 p25	52 → 25 % des patients ont moins de 52 ans
 p50	65 → 50 % ont moins de 65 ans (médiane)
 p75	73 → 75 % ont moins de 73 ans
 p100	84 → le plus âgé a 84 ans
'''


# Variables table patients 
vars <- c("age", "IMC", "sexe", "fumeur", "groupe_social", "commune")

#table one fait un groupe by et summarise en calculant la moyen et autre
table1 <- CreateTableOne(vars = vars, strata = "suivi_pharma", data = patiens_df, factorVars = c("sexe", "fumeur", "groupe_social", "commune")) #factor var pour facteur categoriel
# Afficher la table avec les tests de comparaison
print(table1, showAllLevels = TRUE, test = TRUE)
'''
  Variable	    Résultat global	                                Interprétation
  Nombre de patients	102 non suivis, 98 suivis	                Échantillon équilibré 
  Âge	          Moyenne très proche (62.4 vs 63.6), p = 0.51	  Pas de différence significative
  IMC	          Exactement le même (27.61), p = 1.00	          Aucune différence
  Sexe	        Un peu plus de femmes chez les suivis p = 0.32	Non significatif
  Fumeur	      Légèrement moins de fumeurs dans le groupe suivi, p = 0.31	Non significatif
  Groupe social	Répartition quasi identique, p = 0.879        	Non significatif
  Commune	      Distribution assez homogène, p = 0.492	        Pas de différence notable
'''
t.test(age ~ suivi_pharma, data = patiens_df)

#analyse patients consult 
skim(patients_consult)


vars2 <- c("HbA1c","tension_sys","tension_dia", "poids", "glycémie")
table2<- CreateTableOne(vars = vars2, strata = "suivi_pharma", data = patients_consult, factorVars = NULL)
print(table2, showAllLevels = TRUE, test=TRUE)
'''
                         Stratified by suivi_pharma
                          level 0              1              p      test
  n                                366            340                    
  HbA1c (mean (SD))               7.45 (1.21)    7.52 (1.23)   0.460     
  tension_sys (mean (SD))       134.18 (13.91) 133.99 (14.27)  0.852     
  tension_dia (mean (SD))        85.08 (8.81)   84.51 (8.56)   0.389     
  poids (mean (SD))              75.23 (12.49)  75.42 (11.71)  0.834     
  glycémie (mean (SD))          157.22 (31.45) 159.95 (30.46)  0.243  
'''

t.test(HbA1c ~ suivi_pharma, data = patients_consult)

#visualisation du t test;
patients_consult$suivi_pharma <- factor(patients_consult$suivi_pharma, levels = c(0,1), labels = c("Non suivi", "Suivi"))

ggplot(patients_consult, aes(x = suivi_pharma, y = HbA1c, fill = suivi_pharma)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "black") +
  labs(title = "HbA1c selon le suivi pharmaceutique",
       subtitle = "Comparaison des moyennes (t-test)",
       x = "Suivi pharmaceutique",
       y = "HbA1c (%)") +
  theme_minimal() +
  scale_fill_manual(values = c("#F8766D", "#00BFC4")) +
  theme(legend.position = "none")


################################Analyse uni et bivariee:###############################----

'''
Comparaison des moyennes de HbA1c entre les groupes :
  Moyenne HbA1c ≈ 7.45 (non suivis) vs 7.52 (suivis) → pas significatif (p = 0.46)
  t-test et régression linéaire simple → aucune différence significative
  
Comparaison des proportions de patients avec HbA1c < 7 % :
  Régression logistique simple → OR = 0.87, p = 0.37 → non significatif
  
on ne fait pas d anova dans ce cas car on a exactement 2 groupes donc un t-test revien a la meme chose et plus facile a interpreter

'''
# Régression linéaire simple
lm1 <- lm(HbA1c ~ suivi_pharma, data = patients_consult)
summary(lm1)
'''
Call:
lm(formula = HbA1c ~ suivi_pharma, data = patients_consult)

Residuals:
    Min      1Q  Median      3Q     Max 
-3.6212 -0.8530 -0.0212  0.7788  3.9470 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept)   7.45301    0.06397  116.51   <2e-16 ***
suivi_pharma  0.06817    0.09218    0.74     0.46    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.224 on 704 degrees of freedom
Multiple R-squared:  0.0007763,	Adjusted R-squared:  -0.000643 
F-statistic: 0.5469 on 1 and 704 DF,  p-value: 0.4598
'''

# Régression linéaire multiple
lm2 <- lm(HbA1c ~ suivi_pharma + age + IMC + sexe + fumeur + groupe_social, data = patients_consult)
summary(lm2)
'''
Call:
lm(formula = HbA1c ~ suivi_pharma + age + IMC + sexe + fumeur + 
    groupe_social, data = patients_consult)

Residuals:
    Min      1Q  Median      3Q     Max 
-3.5965 -0.8355 -0.0260  0.8086  3.8784 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)   6.758420   0.375926  17.978   <2e-16 ***
suivi_pharma  0.057318   0.093276   0.614    0.539    
age           0.005389   0.003578   1.506    0.133    
IMC           0.008515   0.010609   0.803    0.422    
sexe          0.031481   0.093906   0.335    0.738    
fumeur        0.072159   0.113793   0.634    0.526    
groupe_social 0.051696   0.061818   0.836    0.403    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.224 on 699 degrees of freedom
Multiple R-squared:  0.007308,	Adjusted R-squared:  -0.001213 
F-statistic: 0.8577 on 6 and 699 DF,  p-value: 0.5258
'''




################################# Analyse multivaries: ########################-----

'''
  Regressoin lineaire multiple;
    Effet du suivi pharmaceutique : Coefficient = +0.057, p = 0.539
    Aucun des autres facteurs n’est significatif non plus
    R² ≈ 0.007 → modèle explique très peu de variance
  Regression logistique multiple;
    Effet du suivi pharmaceutique : OR ≈ 0.89, p = 0.489
    Aucun facteur significatif
    AIC ≈ 909 (pas mieux que modèle nul)
'''

#on ajoute a un variavle controle 1-0 pour les patients ayant un HbA1c qui repond au critere de |sain|
patients_consult$bon_controle <- ifelse(patients_consult$HbA1c < 7, 1, 0)

# Logistique simple
glm1 <- glm(bon_controle ~ suivi_pharma, data = patients_consult, family = "binomial")
summary(glm1)

'''
Call:
glm(formula = bon_controle ~ suivi_pharma, family = "binomial", 
    data = patients_consult)

Coefficients:
             Estimate Std. Error z value Pr(>|z|)    
(Intercept)   -0.6202     0.1096  -5.659 1.53e-08 ***
suivi_pharma  -0.1444     0.1599  -0.903    0.367    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 899.68  on 705  degrees of freedom
Residual deviance: 898.86  on 704  degrees of freedom
AIC: 902.86

Number of Fisher Scoring iterations: 4
'''

# Logistique multiple

bon_controle <- patients_consult%>%
  left_join(obs_suivi_hba1c %>%
              select(id_patient, score_observance_final),
            by = "id_patient")

glm2 <- glm(bon_controle ~ suivi_pharma + score_observance_final + age + IMC + sexe + fumeur + groupe_social,
            data = bon_controle, family = "binomial")
summary(glm2)
'''
Call:
glm(formula = bon_controle ~ suivi_pharma + score_observance_final + 
    age + IMC + sexe + fumeur + groupe_social, family = "binomial", 
    data = bon_controle)

Coefficients:
                        Estimate Std. Error z value Pr(>|z|)
(Intercept)             0.262680   0.649866   0.404    0.686
suivi_pharmaSuivi      -0.110477   0.162666  -0.679    0.497
score_observance_final -0.003958   0.026765  -0.148    0.882
age                    -0.009176   0.006267  -1.464    0.143
IMC                    -0.007505   0.018414  -0.408    0.684
sexe                   -0.084256   0.163035  -0.517    0.605
fumeur                  0.076602   0.195912   0.391    0.696
groupe_social          -0.039782   0.107516  -0.370    0.711

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 899.68  on 705  degrees of freedom
Residual deviance: 895.54  on 698  degrees of freedom
AIC: 911.54

Number of Fisher Scoring iterations: 4
'''

###################### ODS RATIO #########################----

#les coefficient sont estime dans le modele de regression logistique (les log-odds en soit) et calcule l exponentielle permet d interpreter en odd ratio(OR)
exp(coef(glm2))
'''
(Intercept) suivi_pharmaSuivi               age               IMC              sexe            fumeur     groupe_social 
   1.2882017         0.8937536         0.9907248         0.9925014         0.9197416         1.0811364         0.9608516 
'''


#################### ANALYSE TEMPS DE SUIVI ##################----
total_duree_suivi

# Transformation éventuelle du temps en mois (optionnel)
total_duree_suivi_hba1c$duree_mois <- total_duree_suivi_hba1c$duree_totale_jours / 30

# Modèle de régression linéaire simple
model_lm_tps_suivi <- lm(HbA1c ~ duree_mois, data = total_duree_suivi_hba1c)
summary(model_lm_tps_suivi)

'''
Call:
lm(formula = HbA1c ~ duree_mois, data = total_duree_suivi_hba1c)

Residuals:
    Min      1Q  Median      3Q     Max 
-3.6206 -0.9105  0.0265  0.7919  3.1271 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept)  7.796068   0.200665  38.851   <2e-16 ***
duree_mois  -0.014545   0.009362  -1.554    0.122 
              |
---           \>chaque mois de suivi est associé à une baisse moyenne de HbA1c de 0.0145 %
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.22 on 193 degrees of freedom
Multiple R-squared:  0.01235,	Adjusted R-squared:  0.007236 => le modèle n explique que 1,2 % de la variance de HbA1c
F-statistic: 2.414 on 1 and 193 DF,  p-value: 0.1219
'''

model_log <- glm(bon_controle ~ duree_mois, family = "binomial", data = total_duree_suivi_hba1c)
summary(model_log)
exp(coef(model_log))  # Pour les OR
'''
Call:
glm(formula = bon_controle ~ duree_mois, family = "binomial", 
    data = total_duree_suivi_hba1c)

Coefficients:
            Estimate Std. Error z value Pr(>|z|)  
(Intercept) -0.86477    0.34920  -2.476   0.0133 *
duree_mois   0.01349    0.01606   0.840   0.4010  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 253.42  on 194  degrees of freedom
Residual deviance: 252.71  on 193  degrees of freedom
AIC: 256.71

Number of Fisher Scoring iterations: 4

> exp(coef(model_log))  # Pour les OR
(Intercept)  duree_mois 
  0.4211489   1.0135807 => Chaque mois de suivi est associé à une augmentation de 1,36 % des chances d’avoir un bon contrôle glycémique, mais cette relation est non significative statistiquement (p = 0.401).

'''

### ajout de la variable duree au model

total_duree_suivi_full <- patients_consult%>%
left_join(total_duree_suivi_hba1c %>%
          select(id_patient, duree_mois),
          by = "id_patient")

model_lm_multi_duree <- lm(HbA1c ~ duree_mois + age + IMC + sexe + fumeur + groupe_social,
                     data = total_duree_suivi_full)
summary(model_lm_multi_duree)
'''
Call:
lm(formula = HbA1c ~ duree_mois + age + IMC + sexe + fumeur + 
    groupe_social, data = total_duree_suivi_full)

Residuals:
    Min      1Q  Median      3Q     Max 
-3.6076 -0.8231 -0.0331  0.7664  3.8224 

Coefficients:
               Estimate Std. Error t value Pr(>|t|)    
(Intercept)    6.960319   0.393727  17.678   <2e-16 *** (significatif)
duree_mois    -0.010916   0.005026  -2.172   0.0302 *   (significatif)
age            0.005709   0.003588   1.591   0.1121    
IMC            0.009152   0.010751   0.851   0.3949    
sexe           0.003977   0.095002   0.042   0.9666    
fumeur         0.084884   0.115680   0.734   0.4633    
groupe_social  0.055337   0.062351   0.888   0.3751    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.22 on 683 degrees of freedom
  (16 observations effacées parce que manquantes)
Multiple R-squared:  0.01424,	Adjusted R-squared:  0.005583 
F-statistic: 1.645 on 6 and 683 DF,  p-value: 0.1322

Le temps de suivi est faiblement mais significativement associé à une baisse du HbA1c dans le modèle linéaire,
même en contrôlant les autres facteurs.
Cela suggère que la durée du suivi pourrait influencer le niveau de HbA1c, 
mais pas de manière assez marquée pour garantir un bon contrôle strict.
'''

model_log_multi_duree <- glm(bon_controle ~ duree_mois + age + IMC + sexe + fumeur + groupe_social,
                       family = "binomial", data = total_duree_suivi_full)

summary(model_log_multi_duree)
'''
Call:
glm(formula = bon_controle ~ duree_mois + age + IMC + sexe + 
    fumeur + groupe_social, family = "binomial", data = total_duree_suivi_full)

Coefficients:
               Estimate Std. Error z value Pr(>|z|)
(Intercept)    0.103198   0.679228   0.152    0.879
duree_mois     0.012438   0.008749   1.422    0.155
age           -0.010072   0.006222  -1.619    0.106
IMC           -0.010574   0.018776  -0.563    0.573
sexe          -0.037597   0.165304  -0.227    0.820
fumeur         0.074314   0.199987   0.372    0.710
groupe_social -0.052502   0.108713  -0.483    0.629

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 882.49  on 689  degrees of freedom
Residual deviance: 876.75  on 683  degrees of freedom
  (16 observations effacées parce que manquantes)
AIC: 890.75

Number of Fisher Scoring iterations: 4
'''


exp(coef(model_log_multi_duree))  # Pour les OR
'''
  (Intercept)    duree_mois           age           IMC          sexe        fumeur groupe_social 
    1.1087106     1.0125152     0.9899784     0.9894817     0.9631011     1.0771451     0.9488522
'''






