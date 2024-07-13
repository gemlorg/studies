<?php
include '../header.php';
?>

<section class="signup-form">
    <h2>Log in</h2> 
    <div class="signup-form-form">
        <form action= "../includes/login.inc.php" method="post">
            <input type="text" name="email" placeholder="Login\Email...">
            <br>
            <input type="password" name="password" placeholder="Password...">
            <br>
            <select name="type">
                <option name="Customer">Customer</option>
                <option name="Seller">Seller</option>
            </select>
            <br>
            <button type="submit" name="submit">Log in</button>
        </form>
    </div>
</section>
<?php
if (isset($_GET["error"])) {
    if ($_GET["error"] == "empty_input") {
    echo "<p>Fill in all fields!</p>";
    } else if ($_GET["error"] =="wrong_input") {
        echo "<p>Invalid Input!</p>";
    } else if ($_GET["error"] =="wrong_password") {
        echo "<p>Wrong Password!</p>";
    } else if ($_GET["error"] =="no_user") {
        echo "<p>No such user!</p>";
    } 
}
?>


<?php 
include 'footer.php';
?>