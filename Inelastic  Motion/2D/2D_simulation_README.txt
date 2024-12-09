
README

2D Ball Simulation with GIF Creation

This Python script simulates the motion of a ball in 2D space under the influence of gravity, air resistance, and friction. The simulation captures frames and converts them into a GIF.

---

Features
- Realistic physics simulation including gravity, air resistance, and friction.
- Ball bounces off walls and ground with energy dissipation.
- Captures the animation frames and generates a GIF at the end.

---

Authors
- Busayomi THOMPSON-AJAYI
- Samuel OGIDI
- Developed with the assistance of OpenAI's ChatGPT.

---

Requirements
1. Python 3.6+
2. Python Libraries:
   - turtle
   - Pillow (Install with `pip install pillow`)
3. Ghostscript (required for converting PostScript frames to images):
   - Windows: Download and install from Ghostscript Official Downloads (https://ghostscript.com/releases/gsdnld.html).
   - macOS: Install using Homebrew:
     ```
     brew install ghostscript
     ```
   - Linux: Install via your package manager:
     ```
     sudo apt-get install ghostscript
     ```

---

Installation and Setup
1. Install Python:
   - Ensure Python is installed on your system. Download it from python.org.

2. Install Required Libraries:
   - Run the following command in your terminal or command prompt:
     ```
     pip install pillow
     ```

3. Install Ghostscript:
   - Windows:
     - Download the appropriate version for your system (32-bit or 64-bit).
     - Install Ghostscript and ensure the `bin` directory (e.g., `C:\Program Files\gs\gs10.01.1\bin`) is added to your system's PATH environment variable.
   - macOS/Linux:
     - Use a package manager like Homebrew (macOS) or APT (Linux) to install Ghostscript.

4. Run the Code:
   - Save the Python script in a file (e.g., `ball_simulation.py`).
   - Run the script:
     ```
     python ball_simulation.py
     ```
   - The program will simulate the ball motion and save the result as `ball_simulation.gif` in the same directory
NOTE: Saving this simulation to GIF might take time. We are still working on optimizing our code :)
It took me about 15 minutes to save to GIF on my laptop.

---

Troubleshooting
- "Unable to locate Ghostscript" Error:
  - Ensure Ghostscript is installed and accessible via your system's PATH.
  - Test the installation by running `gs --version` in the terminal. It should return the installed Ghostscript version.

- Freezing During GIF Conversion:
  - The optimized code avoids disk I/O overhead. Ensure you use the latest code version.

---

README (FRANÇAIS)

Simulation 2D d'une balle avec création de GIF

Ce script Python simule le mouvement d'une balle dans un espace 2D sous l'effet de la gravité, de la résistance de l'air et de la friction. La simulation capture les images et les convertit en un GIF.

---

Fonctionnalités
- Simulation réaliste incluant la gravité, la résistance de l'air et la friction.
- La balle rebondit sur les murs et le sol avec dissipation d'énergie.
- Capture l'animation et génère un GIF à la fin.

---

Auteurs
- Busayomi THOMPSON-AJAYI
- Samuel OGIDI
- Développé avec l'assistance de ChatGPT d'OpenAI.

---

Prérequis
1. Python 3.6+
2. Bibliothèques Python :
   - turtle
   - Pillow (Installer avec `pip install pillow`)
3. Ghostscript (requis pour convertir les images PostScript en GIF) :
   - Windows : Télécharger et installer depuis Ghostscript Official Downloads (https://ghostscript.com/releases/gsdnld.html).
   - macOS : Installer avec Homebrew :
     ```
     brew install ghostscript
     ```
   - Linux : Installer via votre gestionnaire de paquets :
     ```
     sudo apt-get install ghostscript
     ```

---

Installation et Configuration
1. Installer Python :
   - Téléchargez Python depuis python.org.

2. Installer les Bibliothèques Requises :
   - Lancez la commande suivante dans votre terminal ou invite de commande :
     ```
     pip install pillow
     ```

3. Installer Ghostscript :
   - Windows :
     - Téléchargez et installez la version correspondant à votre système (32 bits ou 64 bits).
     - Ajoutez le répertoire `bin` de Ghostscript (ex. : `C:\Program Files\gs\gs10.01.1\bin`) à la variable d'environnement PATH.
   - macOS/Linux :
     - Utilisez un gestionnaire de paquets comme Homebrew (macOS) ou APT (Linux) pour installer Ghostscript.

4. Exécuter le Code :
   - Sauvegardez le script Python dans un fichier (ex. : `ball_simulation.py`).
   - Lancez le script :
     ```
     python ball_simulation.py
     ```
   - Le programme simulera le mouvement de la balle et sauvegardera le résultat sous `ball_simulation.gif` dans le même répertoire.

---

Dépannage
- Erreur "Impossible de localiser Ghostscript" :
  - Assurez-vous que Ghostscript est installé et accessible via le PATH du système.
  - Testez l'installation en lançant `gs --version` dans le terminal. Cela doit retourner la version installée de Ghostscript.

- Blocage lors de la conversion en GIF :
  - Le code optimisé évite les opérations sur disque. Assurez-vous d'utiliser la dernière version du code.
