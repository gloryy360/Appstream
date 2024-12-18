# -*- coding: utf-8 -*-
"""
Created on Fri Nov 29 11:24:49 2024

@author: gadek
"""

import streamlit as st
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
data = pd.read_csv("iris.csv")
st.title("Exploration de la base de données Iris")
st.header("Aperçu des données")
st.write(data.head())

# Sélection des colonnes
st.sidebar.header("Options de visualisation")
columns = st.sidebar.multiselect("Choisissez les colonnes à afficher", data.columns, default=data.columns)
st.write(data[columns].head())

# Visualisation des données
st.header("Visualisations")
chart_type = st.selectbox("Choisissez le type de graphique", ["Histogramme", "Pairplot", "Boxplot"])

if chart_type == "Histogramme":
    column = st.selectbox("Choisissez une colonne", data.select_dtypes(include=['float64', 'int64']).columns)
    plt.figure(figsize=(8, 6))
    sns.histplot(data[column], kde=True)
    st.pyplot(plt)

elif chart_type == "Pairplot":
    plt.figure(figsize=(10, 8))
    sns.pairplot(data, hue="species")
    st.pyplot(plt)

elif chart_type == "Boxplot":
    column = st.selectbox("Choisissez une colonne pour le boxplot", data.select_dtypes(include=['float64', 'int64']).columns)
    plt.figure(figsize=(8, 6))
    sns.boxplot(x="species", y=column, data=data)
    st.pyplot(plt)

# Filtrer les données
st.header("Filtrage des données")
filter_species = st.multiselect("Choisissez les espèces à afficher", options=data["species"].unique(), default=data["species"].unique())
filtered_data = data[data["species"].isin(filter_species)]
st.write(filtered_data)
