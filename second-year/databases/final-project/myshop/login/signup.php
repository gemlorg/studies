<?php
include '../header.php';
?>

<section class="signup-form">
    <h2>Sign Up</h2> 
    <div class="signup-form-form">
        <form action= "/myshop/includes/signup.inc.php" method="post">
            <input type="text" name="name" placeholder="Name...">
            <br>
            <input type="text" name="surname" placeholder="Surname...">
            <br>
            <input type="text" name="username" placeholder="Username...">
            <br>
            <input type="date" name="brith_date" placeholder="Brith Date...">
            <br>
            <input type="text" name="email" placeholder="Email Adress...">
            <br>
            <input type="text" name="adress" placeholder="Home Adress...">
            <br>
            <input type="number" name="card" placeholder="Card Number...">
            <br>
            <input type="password" name="password" placeholder="Password...">
            <br>
            <select name="type">
                <option name="Customer">Customer</option>
                <option name="Seller">Seller</option>
            </select>
            <br>
            <button type="submit" name="submit">Sign Up</button>
        </form>
    </div>
</section>

<?php
if (isset($_GET["error"])) {
    if ($_GET["error"] == "empty_input") {
    echo "<p> Fill in all fields!</p>";
    }else if ($_GET["error"] == "wrong_input") {
        echo "<p> User already exists!</p>";
        }
}

?>
<?php 
include 'footer.php';
?>