<?php 
include 'header.php';
if($_SESSION['type'] != "Seller") {
    header("location: /myshop/index.php");
    exit();
}
?>

<h2>My Sells</h2>

<div class="article-container">
    <?php
        $sql = "SELECT name , SUM(quantity) as quantity, price from Transaction NATURAL JOIN Payment NATURAL JOIN Ord NATURAL JOIN Product WHERE seller_id = '".$_SESSION['user']['seller_id']."' GROUP BY name;";
        $result = mysqli_query($conn, $sql);
        $sum = 0;
        $queryResults = mysqli_num_rows($result) ;
        echo "<table>";
        echo "<tr>
        <th>PRODUCT</th> 
        <th>QUANTITY</th>
        <th>SUM</th>
      </tr>";
        if ($queryResults > 0) {
            while ($row = mysqli_fetch_assoc($result)) {
                echo " <tr>
                <td>".$row['name']."</td>
                <td>".$row['quantity']."</td> 
                <td>".$row['price']."</td> 
                </tr>";
                $sum += $row['quantity'] * $row['price'];
            }
        }
        echo "</table>";
        
        echo "<h2>OVERALL:  $sum</h2>";
    ?>
</div>
<?php

?>


<?php 
include 'footer.php';
?>