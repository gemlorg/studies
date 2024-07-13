<?php
include 'header.php'
?>

<h1> Edit Page</h1>

    <?php
    if (isset($_POST['submit-edit'])) {
        // $product = find_product($conn, $_GET['product_id']);

        
        $pid = $_GET['product_id'];
        $sql = "SELECT * FROM `Product` WHERE product_id = $pid";
        $result = mysqli_query($conn, $sql);
        if ($row = mysqli_fetch_assoc($result)) {
            $product  = $row;
        }else {
            $product = false;

        }
    


    } else {
        header("location: /myshop/index.php");
    }
    echo 
    '<div class="edit-form-form">
        <form action= "/myshop/includes/edit.inc.php?product_id='.$pid.'" method="post">
            <p><h3>Name: '.$product['name'].'</h3></p>
            <br>
            <input type="text" name="name" value= '.$product["name"].'>
            <br>
            <p><h3>Description: '.$product['Description'].'</h3></p>
            <br>
            <input type="text" name="description" value='.$product["description"].'>
            <br>
            <p><h3>Quantity: '.$product['available'].'</h3></p>
            <br>
            <input type="number" name="available" value='.$product["available"].'>
            <br>
            <button type="submit" name="submit">Submit</button>
        </form>
    </div>'

    ?>




<?php 
include 'footer.php';
?>
