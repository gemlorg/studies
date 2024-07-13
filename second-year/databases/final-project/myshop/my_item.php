<?php
echo " <tr>
<td>".$row['name']."</td> 
<td>".$row['available']."</td>
<td>".$row['price']."</td>
<td> 
    <form action='edit_my.php?product_id=".$row['product_id']."' method= 'POST'> 
        <button type='submit' name='submit-edit'>Edit</button>
    </form>
</td>
</tr>";
