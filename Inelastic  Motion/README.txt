
README

Overview
This project simulates the inelastic motion of a glass bead falling under the influence of gravity with dissipation. The simulation data is outputted to a file, and gnuplot is used to visualize results.

---

Instructions for Use

On Linux
1. Open the Project in VS Code:
   - Extract the folder named `Inelastic Motion`.
   - Open the folder in VS Code as the workspace folder.

2. Run the Simulation:
   - Open the terminal in VS Code.
   - Run the following command:
     ./run_simulation.sh
   - The script will run the simulation and generate plots.

---

On Windows
1. Install WSL (Windows Subsystem for Linux):
   - Open PowerShell as Administrator.
   - Install WSL with:
     wsl --install
   - Restart your computer if prompted.
   - Launch a terminal and verify WSL is installed by typing:
     wsl --list --online

2. Set Up Linux Environment:
   - Install Ubuntu (or your preferred Linux distribution) by running:
     wsl --install -d Ubuntu
   - After installation, update the package list:
     sudo apt update
   - Install the required tools:
     sudo apt install gfortran gnuplot

3. Run the Simulation:
   - Navigate to the project folder in WSL.
   - Run the script:
     ./run_simulation.sh

---

French Translation (Français)

Aperçu
Ce projet simule le mouvement inélastique d'une bille en verre tombant sous l'effet de la gravité avec dissipation. Les données de simulation sont exportées vers un fichier, et gnuplot est utilisé pour visualiser les résultats.

---

Instructions d'Utilisation

Sous Linux
1. Ouvrir le Projet dans VS Code :
   - Extrayez le dossier nommé `Inelastic Motion`.
   - Ouvrez ce dossier dans VS Code comme espace de travail.

2. Exécuter la Simulation :
   - Ouvrez le terminal dans VS Code.
   - Exécutez la commande suivante :
     ./run_simulation.sh
   - Le script exécutera la simulation et générera des graphiques.

---

Sous Windows
1. Installer WSL (Sous-système Windows pour Linux) :
   - Ouvrez PowerShell en tant qu'administrateur.
   - Installez WSL avec la commande :
     wsl --install
   - Redémarrez votre ordinateur si demandé.
   - Lancez un terminal et vérifiez l'installation de WSL en tapant :
     wsl --list --online

2. Configurer l'Environnement Linux :
   - Installez Ubuntu (ou votre distribution Linux préférée) en exécutant :
     wsl --install -d Ubuntu
   - Après l'installation, mettez à jour la liste des paquets :
     sudo apt update
   - Installez les outils requis :
     sudo apt install gfortran gnuplot

3. Exécuter la Simulation :
   - Accédez au dossier du projet dans WSL.
   - Lancez le script :
     ./run_simulation.sh
