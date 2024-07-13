# Project Assignment

The task involves creating a simple data warehouse. The data warehouse should have a specific application. (A standard example: a pizza ordering application. Two interfaces will be needed (for employees and customers). The database will store the inventory levels of the ingredients. An order will be possible when all ingredients are available, etc.)

## Team

The project can be done individually or in pairs.

## Interface

The data warehouse must have a web interface. When designing the interface, focus on simplicity and user-friendliness. It must enable:

- Displaying data in tabular form,
- Searching and filtering data in the table,
- Adding, editing, and deleting data.

## Technical Requirements

- The database must contain at least 5 entities.
- There must be at least 6 relationships between the entities.
- The project must use PL/SQL.

## Evaluation Criteria

Since the application is an assignment for the Databases course, all database-related aspects will be evaluated in the final project. These include:

- SQL queries (correctness, efficiency, complexity),
- Use of appropriate database functions (security - e.g., resistance to SQL Injection),
- Ensuring database consistency.

The quality of the interface code will not be evaluated, and the only requirement for its appearance is clarity.

## Project Stages

### Stage 1: Entity Relationship Model Submission (December 6, 2022)

To pass the first stage, you need to send an electronic version of the entity relationship model. The only requirement for the model is its readability and the inclusion of information about entities, attributes (whether it is a primary key, a foreign key, or mandatory), and relationships between entities (cardinality, mandatory).

Attach a short description of the system for which the model was created. For example, "This schema represents a pizzeria. The database contains information about possible pizzas, inventory status, and purchase history. The order status is available from the customer interface..." etc.

### Stage 2: Database Creation Script Submission (December 22, 2022)

The second stage involves preparing a script that will create the system's database. This script should contain queries to create all tables in the database with the appropriate constraints and at least one non-trivial procedure/function or trigger (it is also worth including auto-increment functions).

### Stage 3: Final Application Presentation (January 24, 2023)

The third stage involves presenting the completed application in a computer lab - demonstrating its functionality and the code.
