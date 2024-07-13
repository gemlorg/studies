<?php
include '../header.php';
if($_SESSION['type'] != "Seller") {
    header("location /myshop/index.php");
}
?>

<section class="signup-form">
    <h2>Sell</h2> 
    <div class="signup-form-form">
        <form action= "/myshop/sell/new.php" method="post">
            <select name="type">
                <option name="new">Sell New</option>
                <option name="existing">Sell Existing</option>
            </select>
            <button type="submit" name="submit">Continue</button>
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
include '/myshop/footer.php';
?>