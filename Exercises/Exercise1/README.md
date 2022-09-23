# Séance d'exercices 1

La séance d'exercices d'aujourd'hui est dédiée à la mise en place de l'environnement nécessaire pour programmer en F# et à l'apprenstissage du langage.

## Tâche 1

Installez [F#](https://learn.microsoft.com/en-us/dotnet/fsharp/get-started/get-started-command-line) sur votre machine. 

## Tâche 2

Vous trouverez dans le fichier `FSharpIntro.ipynb` une brève introduction aux concepts de base de F#.
Vous pouvez lire ce tutoriel pour vous familiariser avec le langage.
Le document contient également des références vers d'autres ressources plus avancées qui pourront vous aider dans votre apprentissage.

Le document est écrit sous la forme d'un [Jupyter notebook](https://jupyter.org/) que vous pouvez exécuter dans [VSCode](https://code.visualstudio.com/) si vous installez le plugin [.NET interactive](https://marketplace.visualstudio.com/items?itemName=ms-dotnettools.dotnet-interactive-vscode) (mais vous pouvez également simplement le lire, sans exécuter les cellules de code qu'il contient).

## Tâche 3

Un fois que vous serez un peu plus à l'aise avec F#, essayez de compléter le code fourni dans le dossier `Exercise` de ce repository.
Le dossier contient un projet .NET écrit en F# qui implémente un interpréteur pour un mini-langage de programmation.

L'interpréteur permet à l'utilisateur de rentrer interactivement des commandes à exécuter.
Une commande permet soit de définir une nouvelle variable `x` en entrant du texte de la forme `let x = expr` où `expr` est une expression du langage et `x` un nom de variable, soit d'exécuter directement une expression.
Les expressions disponibles sont le `if/then/else`, les opérations binaires `or`, `and`, `=`, `<` et `+`, les opérations unaires `not` et `-`, la lecture de la valeur d'une variable, ainsi que les litéraux entiers et booléens.

Tout le code qui définit la structure des commandes du langage et permet de les *parser* est déjà fourni. 
Vous devez simplement compléter la fonction `eval` du fichier `Core.fs`, qui permet d'évaluer des expressions. 

L'objectif de cet exercice est de vous entrainer à vous familiariser avec une *code base* écrite en F# et d'essayer d'appliquer les notions vues dans le petit tutoriel de la tâche précédente.
