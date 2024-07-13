Based on the analysis of the project's files and the provided assignment description, here is a README that succinctly captures the project's purpose and setup:

---

# Compiler Django Project

This project is a Django web application designed to facilitate the compilation of programs for 8-bit processors using the SDCC compiler. The application allows users to upload or edit C code files, set compiler options, and retrieve the compiled output.

## Purpose

The primary goal of this project is to create a web-based interface that enables users to compile C code for 8-bit processors. The application is intended to support the following functionalities:

- **File Upload and Management**: Users can upload C code files or edit them directly within the application.
- **Compiler Options**: Users can configure various compiler options through a user-friendly interface.
- **Code Compilation**: The application utilizes the SDCC compiler to compile the uploaded or edited C code and provides the compiled output to the user.
- **Real-time Code Display**: The interface includes areas for displaying the full program text and selected code fragments, with options to view different compiler settings.

## Features

- **User Authentication**: Secure login and logout functionality.
- **File Management**: Upload, edit, and manage C code files.
- **Compilation Settings**: Tabs for configuring processor type, compiler standards, and other relevant options.
- **Compilation Output**: Retrieve and view the compiled output of the code.
- **Responsive Design**: Optimized for various screen resolutions and devices.

## Installation and Setup

1. **Clone the repository**:

   ```bash
   git clone <repository-url>
   cd mysite
   ```

2. **Install Dependencies**:
   Ensure all necessary dependencies are installed, including `sdcc`.

3. **Database Migrations**:
   Apply the database migrations to set up the database schema.

   ```bash
   python manage.py migrate
   ```

4. **Run the Development Server**:
   Start the Django development server to run the project locally.

   ```bash
   python manage.py runserver
   ```

5. **Access the Application**:
   Open your web browser and go to `http://127.0.0.1:8000/` to access the application.

## Testing

To run tests and check test coverage:

```bash
python manage.py test
coverage run manage.py test
coverage html
```

## Notes

- Ensure `sdcc` is installed in your environment as it is required for compiling C code.
