# Use Case Diagram Generator (Haskell)

## Table of Contents

- [Project Description](#project-description)
- [Technologies Used](#technologies-used)
- [How it Works](#how-it-works)
- [Features](#features)
- [How to Run](#how-to-run)
- [Example Workflow](#example-workflow)

## Project Description

This Haskell project provides an interactive console-based system to define Use Case Diagram specifications and generate corresponding PlantUML code. Users can define actors, use cases, a system (package) name, and relationships dynamically through a menu-driven interface. The program validates relationship specifications and generates `.puml` files for visualizing the diagrams using PlantUML.

The main goal of this project is to simplify the process of creating UML Use Case Diagrams by providing a user-friendly interactive interface and automating the generation of PlantUML code using Haskell's functional programming paradigm.

## Technologies Used

- **Haskell (GHC)**: For functional programming, data type definitions, and program logic. [cite: 1]
- **PlantUML**: For rendering UML diagrams from the generated `.puml` files. [cite: 1]

## How it Works

The program follows these steps:
1.  **Define Data Structures**: Haskell data types are used to represent actors, use cases, packages (a system name), and relationships. [cite: 1]
2.  **Interactive Menu**: Users interact with the program through a console-based menu system to build the diagram specification.
3.  **Insert Elements & Relationships**: Users can add actors, use cases, set a system name, and define relationships (e.g., associations, generalizations, includes, and extends). [cite: 4]
4.  **Validate Input**: The program validates each relation specification, verifying if the use cases involved are already defined. [cite: 5] It also prevents duplicate elements and relationships.
5.  **Generate PlantUML Code**: Users can generate `.puml` files containing the PlantUML syntax for the defined diagram. [cite: 2, 6]

## Features

-   **Interactive Menu**: A user-friendly console menu system for defining the diagram specification.
-   **Data Definition**: Functions to specify actors, use cases, a system (package) name, and various relationships. [cite: 4]
-   **Relationship Handling**: Supports associations, includes, extends, and generalizations for both actors and use cases.
-   **Validation**: Ensures that use cases involved in relationships are already defined before a relationship is added. [cite: 5] Prevents duplicate entries for elements and relationships.
-   **Diagram Export**: Generates `.puml` files for visualization with PlantUML. [cite: 6]

## How to Run

1.  **Install Haskell (GHC)**: Ensure that you have the Glasgow Haskell Compiler (GHC) and the Cabal or Stack build tools installed. If not, you can download them from [haskell.org/ghcup/](https://www.haskell.org/ghcup/).

2.  **Obtain the Code**:
    * Save the Haskell code into a file, for example, `Main.hs`. [cite: 3]

3.  **Compile the Program**:
    * Open your terminal or command prompt.
    * Navigate to the directory where you saved `Main.hs`.
    * Compile the code using GHC:
        ```bash
        ghc Main.hs
        ```
        This will create an executable file (e.g., `Main` or `Main.exe`).

4.  **Run the Program**:
    * Execute the compiled program from your terminal:
        * On Windows: `.\Main.exe`
        * On macOS/Linux: `./Main`

5.  **Interactive Menu**:
    * The program will start with an interactive welcome message and an initial menu.
    * Follow the prompts to:
        * Create a new diagram.
        * Set a system (package) name.
        * Add actors and use cases.
        * Define relationships between elements.
        * Generate a `.puml` file.

6.  **Generate the PlantUML Diagram**:
    * After defining the diagram through the interactive menus, choose the option to generate a `.puml` file. [cite: 6]
    * The program will prompt for a filename or use a default.
    * Open the generated `.puml` file in a PlantUML-compatible tool (e.g., PlantUML web server, VS Code extension, standalone JAR) to visualize the diagram.

## Example Workflow

1.  **Compile and run the program**:
    * Navigate to the source directory in your terminal.
    * Compile: `ghc Main.hs`
    * Run: `./Main` (or `.\Main.exe` on Windows)

2.  From the **Initial Menu**, choose `1. Create a new diagram`.
3.  **Enter a system name** when prompted (e.g., "Online Shopping System").
4.  From the **Main Menu**:
    * Choose `1. Manage Elements (Actors, Use Cases)`:
        * Select `1. Add Actor` and add "Customer".
        * Select `1. Add Actor` again and add "Administrator".
        * Select `2. Add Use Case` and add "Browse Products".
        * Select `2. Add Use Case` and add "Place Order".
        * Select `2. Add Use Case` and add "View Order History".
        * Select `2. Add Use Case` and add "Manage Products".
        * Select `0. Back to Main Menu`.
    * Choose `2. Manage Relationships`:
        * Select `1. Add Association (Actor -- Use Case)`:
            * Select "Customer" as the actor.
            * Select "Browse Products" as the use case.
        * Select `1. Add Association (Actor -- Use Case)` again:
            * Select "Customer" as the actor.
            * Select "Place Order" as the use case.
        * Select `2. Add <<include>> Relation`:
            * Select "Place Order" as the base use case.
            * Select "View Order History" as the included use case (example: order placement includes updating/showing history).
        * Select `0. Back to Main Menu`.
5.  From the **Main Menu**, choose `4. Generate .puml Diagram File`.
    * Enter a filename or press Enter for the default.
6.  **Visualize**: Open the generated `.puml` file with a PlantUML tool.
7.  From the **Main Menu**, choose `0. Exit to Initial Menu`, then `0. Exit Program` to quit.
