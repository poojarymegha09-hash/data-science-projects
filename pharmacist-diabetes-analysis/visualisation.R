#packages
library(readr)
library(tibble)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)

#nbre consultation par patient
nb_consult_par_patient <- consultations_df %>%
  group_by(id_patient) %>%
  summarise(nb_consultations = n())

nb_consult_par_patient

# Joindre pour récupérer le statut de suivi
consult_suivi <- nb_consult_par_patient %>%
  left_join(patiens_df %>% 
  select(id_patient, suivi_pharma), by = "id_patient")

# Comparer le nombre de consultations entre suivis et non suivis
comparaison_suivi <- consult_suivi %>%
  group_by(suivi_pharma) %>%
  summarise(
    moyenne_consult = mean(nb_consultations),
    mediane_consult = median(nb_consultations),
    nb_patients = n()
  )
#visualisation des data comparaison_suivi
comparaison_suivi$suivi_pharma <- factor(comparaison_suivi$suivi_pharma, 
                                         levels = c(0, 1), 
                                         labels = c("Non suivi", "Suivi"))

# Barplot des moyennes avec les médianes en labels
ggplot(comparaison_suivi, aes(x = suivi_pharma, y = moyenne_consult, fill = suivi_pharma)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  geom_text(aes(label = paste0("Médiane: ", mediane_consult)),
            vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("Non suivi" = "#ff7f0e", "Suivi" = "#1f77b4")) +
  labs(
    title = "Nombre moyen de consultations par groupe de suivi",
    x = "Groupe",
    y = "Nombre moyen de consultations",
    fill = "Suivi pharmaceutique"
  ) +
  theme_minimal(base_size = 14)

#groupe social
ggplot(patiens_df, aes(x = groupe_social, y = hba1c_final, fill = groupe_social)) 
+
  geom_boxplot() +
  labs(title = "HbA1c selon le groupe social",
       x = "Groupe social", y = "HbA1c (%)") +
  theme_minimal()
#imc et hbaic
ggplot(patients_consult, aes(x = IMC, y = hba1c_final)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Relation entre IMC et HbA1c",
       x = "IMC (kg/m²)", y = "HbA1c (%)") +
  theme_minimal()

#S Tension artérielle vs HbA1c
ggplot(patients_consult, aes(x = tension_sys_final, y = hba1c_final)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Relation entre tension systolique et HbA1c",
       x = "Tension systolique (mmHg)", y = "HbA1c (%)") +
  theme_minimal()

# HbA1c selon le tabagisme
ggplot(patients_consult, aes(x = fumeur, y = hba1c_final, fill = fumeur)) +
  geom_boxplot() +
  labs(title = "HbA1c selon le statut tabagique",
       x = "Fumeur", y = "HbA1c (%)") +
  theme_minimal()

# Glycémie vs HbA1c
ggplot(patients_consult, aes(x = glycemie_final, y = hba1c_final)) +
  geom_point(alpha = 0.5, color = "purple") +
  geom_smooth(method = "lm", se = TRUE, color = "darkblue") +
  labs(title = "Relation entre glycémie et HbA1c",
       x = "Glycémie (mg/dL)", y = "HbA1c (%)") +
  theme_minimal()

#moyen des repartition caracteristique entre groupe suivi/non suivi
patients_consult %>%
  group_by(suivi_pharma) %>%
  summarise(
    age_moyen = mean(age, na.rm = TRUE),
    proportion_hommes = mean(sexe == '1', na.rm = TRUE)*100,
    milieu_social_moyen = mean(groupe_social, na.rm = TRUE),
    proportion_fumeurs = mean(fumeur == '1', na.rm = TRUE)*100,
  )

######################### GLYC VS HbA1c AVANT/APRES POUR SUIVI/NON SUIVI ####################----
# Trier par id_patient et date_consult
df <- patients_consult %>%
  arrange(id_patient, date_consult)

# Renommer les colonnes pour éviter problème avec accents
df <- df %>%
  rename(glycemie = glycémie, hba1c = HbA1c)

# Extraire la première et dernière visite pour chaque patient
first_last <- df %>%
  group_by(id_patient) %>%
  slice(c(1, n())) %>%
  mutate(visit = ifelse(row_number() == 1, "first", "last")) %>%
  ungroup()

# Transformer les données en format large
wide_df <- first_last %>%
  pivot_wider(
    id_cols = c(id_patient, suivi_pharma),
    names_from = visit,
    values_from = c(glycemie, hba1c)
  )

# Résumé par groupe de suivi
summary_stats <- wide_df %>%
  group_by(suivi_pharma) %>%
  summarise(
    glycemie_avant_moy = mean(glycemie_first, na.rm = TRUE),
    glycemie_apres_moy = mean(glycemie_last, na.rm = TRUE),
    hba1c_avant_moy = mean(hba1c_first, na.rm = TRUE),
    hba1c_apres_moy = mean(hba1c_last, na.rm = TRUE),
    
    glycemie_avant_sd = sd(glycemie_first, na.rm = TRUE),
    glycemie_apres_sd = sd(glycemie_last, na.rm = TRUE),
    hba1c_avant_sd = sd(hba1c_first, na.rm = TRUE),
    hba1c_apres_sd = sd(hba1c_last, na.rm = TRUE)
  )

print(summary_stats)

#graph representation du resume valeur glycemie
summary_long <- summary_stats %>%
  mutate(suivi_pharma = factor(suivi_pharma, levels = c(0, 1), labels = c("Non suivi", "Suivi"))) %>%
  pivot_longer(
    cols = -suivi_pharma,
    names_to = c("variable", "moment", "stat"),
    names_pattern = "(glycemie|hba1c)_(avant|apres)_(moy|sd)"
  ) %>%
  pivot_wider(
    names_from = stat,
    values_from = value
  ) %>%
  mutate(
    variable = recode(variable,
                      glycemie = "Glycémie",
                      hba1c = "HbA1c"),
    moment = recode(moment,
                    avant = "Avant",
                    apres = "Après")
  )
summary_long$moment <- factor(summary_long$moment, levels = c("Avant", "Après"))

#PLOT
ggplot(summary_long, aes(x = moment, y = moy, fill = suivi_pharma)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = moy - sd, ymax = moy + sd),
                position = position_dodge(width = 0.7),
                width = 0.2) +
  facet_wrap(~ variable, scales = "free_y") +
  scale_fill_manual(
    values = c("Suivi" = "#1f77b4", "Non suivi" = "#ff7f0e")
  ) +
  labs(
    title = "Évolution de la Glycémie et de l'HbA1c par groupe de suivi",
    x = "Moment de la visite",
    y = "Valeur moyenne",
    fill = "Groupe"
  ) +
  theme_minimal(base_size = 14)


################ NBRE TRAITEMENT/PATIENT GROUPE SUIVI/NON SUIVI ####################----
# Joindre pour récupérer le statut de suivi
traitement_par_patien_par_groupe <-traitement_patien%>%
  left_join(patiens_df %>%
              select(id_patient, suivi_pharma), by= "id_patient")

traitement_par_patien_par_groupe <- traitement_par_patien_par_groupe %>%
  mutate(suivi_pharma = factor(suivi_pharma, levels = c(0, 1), labels = c("Non suivi", "Suivi")))

# Calculer la moyenne des traitements par groupe
moyennes_traitements <- traitement_par_patien_par_groupe %>%
  group_by(suivi_pharma) %>%
  summarise(
    moyenne_traitements = mean(nb_traitements),
    sd_traitements = sd(nb_traitements),
    n = n()
  )

#PLOT
ggplot(moyennes_traitements, aes(x = suivi_pharma, y = moyenne_traitements, fill = suivi_pharma)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  geom_errorbar(aes(ymin = moyenne_traitements - sd_traitements, ymax = moyenne_traitements + sd_traitements),
                width = 0.2) +
  geom_text(aes(label = round(moyenne_traitements, 2)), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("Non suivi" = "#ff7f0e", "Suivi" = "#1f77b4")) +
  labs(
    title = "Nombre moyen de traitements par groupe de suivi",
    x = "Suivi pharmaceutique",
    y = "Nombre moyen de traitements",
    fill = "Groupe"
  ) +
  theme_minimal(base_size = 14)

##################### TEMPS MOYEN DE TRAITEMENT PAR GROUPE ##################----
#!!! nous partons du principe que les dates de consultation sont lieees au changement de traitement poour les patients !!!

#import des traitement
tps_traitement_groupe<-traitements_df%>%
  left_join(patiens_df %>%
              select(id_patient, suivi_pharma), by = "id_patient")

#import des date de suivi de consultation
derniere_consultation_df <- consultations_df %>%
  group_by(id_patient) %>%
  summarise(date_derniere_consult = max(date_consult, na.rm = TRUE))

#ajout du temps pour la dernier consultation
tps_traitement_groupe<- tps_traitement_groupe %>%
  left_join(derniere_consultation_df, by = "id_patient")

#drop la date de consult 
tps_traitement_groupe <- tps_traitement_groupe %>%
  select(-date_consult)

#rempace les na de fin de traitement avec la date de la derniere consultation
tps_traitement_groupe$debut_traitement <- as.Date(tps_traitement_groupe$début_traitement)
tps_traitement_groupe$fin_traitement <- as.Date(tps_traitement_groupe$fin_traitement)
tps_traitement_groupe$date_derniere_consult <- as.Date(tps_traitement_groupe$date_derniere_consult)

tps_traitement_groupe <- tps_traitement_groupe %>%
  mutate(
    fin_traitement = ifelse(
      is.na(fin_traitement) & date_derniere_consult > debut_traitement,
      date_derniere_consult,
      fin_traitement
    ),
    fin_traitement = as.Date(fin_traitement), 
    durée_jours = as.numeric(fin_traitement - debut_traitement)
  )

#additionne les valeurs par id pour avoir un temps de suivi / patient
#!!!! ici on dorp les na pour les valeurs ou la date de debut de traitement est superieur a la date de derniere consultation!!!!!
total_duree_suivi <- tps_traitement_groupe %>%
  group_by(id_patient) %>%
  summarise(duree_totale_jours = sum(durée_jours, na.rm = TRUE),
            suivi_pharma = first(suivi_pharma)
            ) %>%
  arrange(desc(duree_totale_jours))

#calcule moyen des temps en fonction des groupes

total_duree_suivi <- total_duree_suivi %>%
  mutate(suivi_pharma = factor(suivi_pharma, levels = c(0, 1), labels = c("Non suivi", "Suivi")))

moyennes_suivi <- total_duree_suivi %>%
  group_by(suivi_pharma) %>%
  summarise(duree_moyenne = mean(duree_totale_jours, na.rm = TRUE))


#PLOT
ggplot(moyennes_suivi, aes(x = factor(suivi_pharma), y = duree_moyenne, fill = factor(suivi_pharma))) +
  geom_col(width = 0.5) +
  labs(
    x = "Suivi pharmaceutique",
    y = "Durée moyenne de suivi (jours)",
    title = "Durée moyenne de suivi par groupe de suivi pharmaceutique",
    fill = "Suivi pharma"
  ) +
  scale_fill_manual(
    values = c("Suivi" = "#1f77b4", "Non suivi" = "#ff7f0e")
  )+
  scale_x_discrete(labels = c("0" = "Non suivi", "1" = "Suivi")) +
  theme_minimal()

#plot le temps de suivi avec la HbA1c a la fin du traitement 
derniere_hba1c <- consultations_df %>%
  filter(!is.na(HbA1c)) %>%
  arrange(id_patient, desc(date_consult)) %>%  # on trie par date décroissante
  group_by(id_patient) %>%
  slice(1) %>%  # on prend la dernière ligne par patient
  ungroup() %>%
  select(id_patient, HbA1c)


#plot le temps de suivi avec la HbA1c a la fin du traitement 
total_duree_suivi_hba1c <- total_duree_suivi %>%
  left_join(consultations_df%>%
              select(HbA1c, id_patient), by = "id_patient")

total_duree_suivi_hba1c <- total_duree_suivi_hba1c %>%
  group_by(id_patient) %>%
  slice_tail(n = 1) %>%  
  ungroup()%>%
  filter(duree_totale_jours > 0)

ggplot(total_duree_suivi_hba1c, aes(x = duree_totale_jours, y = HbA1c, color = suivi_pharma)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  labs(
    title = "Relation entre durée de suivi et HbA1c finale",
    x = "Durée totale de suivi (jours)",
    y = "HbA1c finale"
  ) +
  scale_color_manual(
    values = c("Suivi" = "#1f77b4", "Non suivi" = "#ff7f0e")
  ) +
  theme_minimal()


######################### POIDS ##########################----
#Demontre que la variable poids ne pas analysable
#(mesures non fiables, erreurs d’entrée, patients avec beaucoup de variations réelles ?)
poids<-consultations_df%>%
  select(id_patient, poids)

poids_var_patient <- poids %>%
  group_by(id_patient) %>%
  summarise(
    n_mesures = n(),
    poids_sd = sd(poids, na.rm = TRUE),
    poids_min = min(poids, na.rm = TRUE),
    poids_max = max(poids, na.rm = TRUE),
    poids_range = poids_max - poids_min
  ) %>%
  ungroup()

#amplitude/variance max min du poid moyen pour patient
ggplot(poids_var_patient, aes(y = poids_range)) +
  geom_boxplot(fill = "#ff7f0e", alpha = 0.7) +
  labs(
    title = "Distribution de la variation de poids par patient",
    y = "Amplitude (max - min) du poids [kg]"
  ) +
  theme_minimal()

#variation du poid en fonction du nombre de mesure realise par patient
ggplot(poids_var_patient, aes(x = n_mesures, y = poids_sd)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 10, linetype = "dashed", color = "#1f77b4") +
  labs(
    title = "Écart-type du poids selon le nombre de mesures",
    x = "Nombre de mesures par patient",
    y = "Écart-type du poids [kg]"
  ) +
  theme_minimal()


poids_var_patient <- poids %>%
  group_by(id_patient) %>%
  summarise(
    poids_range = max(poids, na.rm = TRUE) - min(poids, na.rm = TRUE),
    .groups = 'drop'
  )

# Moyenne des amplitudes
mean_amplitude <- mean(poids_var_patient$poids_range, na.rm = TRUE)
median_amplitude <- median(poids_var_patient$poids_range, na.rm = TRUE)

mean_amplitude
median_amplitude



########################## OBRSERVANCE HBA1C SUIVI ########################
#obj voir si un bon score d'observance impacte de hba1c en fonction des groupes suivi-non suivi

#tableau avec id + obsevrance 
obs_id <- questionnaire_df%>%
  select(id_patient, score_observance)

obs_id <- obs_id %>%
  group_by(id_patient) %>%
  arrange(id_patient, .by_group = TRUE) %>%
  summarise(
    score_observance_final = case_when(
      n() == 1 ~ score_observance[1],
      n() == 2 & (score_observance[2] - score_observance[1]) > 3 ~ score_observance[2],
      n() == 2 ~ mean(score_observance, na.rm = TRUE),
      TRUE ~ mean(score_observance, na.rm = TRUE)  
    ),
    .groups = "drop"
  )

#tablau avec id + hba1c et extraire derniere valeur pour chaque id
hba1c_id <- consultations_df %>%
  select(id_patient, HbA1c, date_consult) %>%
  arrange(id_patient, date_consult) %>%
  group_by(id_patient) %>%
  slice_tail(n = 1) %>%
  ungroup()

#tableau rasemble id + hba1c + obsevance + suivi
obs_suivi_hba1c <- obs_id%>%
  left_join(hba1c_id, by = "id_patient") %>%
  left_join(patiens_df %>%
              select(id_patient, suivi_pharma),
            by = "id_patient")
#PLOT
ggplot(obs_suivi_hba1c, aes(x = score_observance_final, y = HbA1c, color = factor(suivi_pharma))) +
  geom_point(alpha = 0.7, size = 2.5) +
  geom_smooth(method = "lm", se = FALSE, linetype = "solid") +
  scale_color_manual(
    values = c("0" = "#ff7f0e", "1" = "#1f77b4"),
    labels = c("0" = "Sans suivi pharma", "1" = "Avec suivi pharma"),
    name = "Suivi pharmaceutique"
  ) +
  labs(
    title = "Relation entre observance et HbA1c selon le suivi pharmaceutique",
    x = "Score d'observance",
    y = "HbA1c (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

questionnaire_suivi <- questionnaire_df%>%
  left_join(patiens_df %>%
              select(id_patient, suivi_pharma),
            by = "id_patient")

questionnaire_suivi$suivi_pharma <- as.factor(questionnaire_suivi$suivi_pharma)

ggplot(questionnaire_suivi, aes(x = suivi_pharma, y = score_EQ5D, fill = suivi_pharma)) +
  geom_boxplot() +
  labs(
    title = "Score de qualité de vie (EQ5D)",
    x = "Suivi pharmaceutique",
    y = "Score EQ5D"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Non suivi" = "#ff7f0e", "Suivi" = "#1f77b4")) +
  theme(legend.position = "none")


################### EA5D ET HBA1C ##################----
#montre si le niveau de hba1c a un impact sur le score ea5d

ea5d_id<-questionnaire_df%>%
  group_by(id_patient) %>%
  arrange(id_patient, .by_group = TRUE) %>%
  summarise(
    score_ea5b_final = case_when(
      n() == 1 ~ score_EQ5D[1],
      n() == 2 & (score_EQ5D[2] - score_EQ5D[1]) > 0.15 ~ score_EQ5D[2],
      n() == 2 ~ mean(score_EQ5D, na.rm = TRUE),
      TRUE ~ mean(score_EQ5D, na.rm = TRUE) 
    ),
    .groups = "drop"
  )

ea5d_hba1c <- hba1c_id%>%
  left_join(ea5d_id, by = "id_patient") %>%
  left_join(patiens_df %>%
              select(id_patient, suivi_pharma),
            by = "id_patient")

#PLOT
ggplot(ea5d_hba1c, aes(x = HbA1c, y = score_ea5b_final)) +
  geom_point(aes(color = factor(suivi_pharma)), alpha = 0.7, size = 2) +  
  geom_smooth(method = "lm", se = TRUE, color = "#1f77b4", linetype = "dashed") +
  geom_smooth(method = "loess", se = TRUE, color = "#ff7f0e")+
  labs(
    title = "Relation entre HbA1c et le score EA5B",
    x = "HbA1c (%)",
    y = "Score EA5B final",
    color = "Suivi Pharmaceutique"
  ) +
  theme_minimal()


total_duree_suivi_hba1c$bon_controle <- as.numeric(total_duree_suivi_hba1c$HbA1c < 7)

ggplot(total_duree_suivi_hba1c, aes(x = duree_totale_jours, y = bon_controle)) +
  geom_jitter(height = 0.05, width = 0, alpha = 0.3) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE,
              color = "#1f77b4") +
  labs(title = "Probabilité d’un bon contrôle glycémique selon la durée du suivi",
       x = "Durée du suivi (mois)",
       y = "Probabilité estimée (HbA1c < 7%)") +
  theme_minimal()

#proportion de patient avec un bon control de HbA1c (<7%)
patients_consult$bon_controle_label <- ifelse(patients_consult$HbA1c < 7, "HbA1c < 7%", "HbA1c ≥ 7%")

ggplot(patients_consult, aes(x = suivi_pharma, fill = bon_controle_label)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion de patients avec bon contrôle glycémique",
       subtitle = "HbA1c < 7% selon le suivi pharmaceutique",
       x = "Suivi pharmaceutique",
       y = "Pourcentage",
       fill = "Contrôle glycémique") +
  theme_minimal() +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e"))
