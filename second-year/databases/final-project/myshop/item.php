<?php
echo"<a href='buy.php?product_id=".$row['product_id']."'> <div = 'article-box'> 
                <h3>".$row['name']."</h3> </a>
                <p>".$row['description']."</p>
                <a href='index.php?category=".$row['category']."'> <p>".$row['category']."</p></a>
                <p>".$row['price']."</p>
                </div>";