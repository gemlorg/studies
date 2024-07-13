CREATE TABLE Customer (
  customer_id integer primary key,
  name varchar(20),
  surname varchar(20),
  hash integer,
  adress varchar(100),
  birth_date date,
  email varchar(40),
  card integer
);
CREATE TABLE Seller (
  seller_id integer primary key,
  name varchar(20),
  surname varchar(20),
  nip integer,
  card integer,
  adress varchar(40)
);
CREATE TABLE Payment (
  payment_id integer primary key,
  "date" date,
  amount integer,
  seller_id integer references Seller(seller_id),
  customer_id integer  references Customer(customer_id)
);
CREATE TABLE Category (
  category_id integer primary key,
  name varchar(40),
  description varchar(200)
);
CREATE TABLE Product (
  product_id integer primary key,
  available integer,
  description varchar(200),
  price integer,
  category_id integer references Category(category_id) 
);
CREATE TABLE Post (
  post_id integer primary key,  
  name varchar(40),
  price_km integer
);
CREATE TABLE Delivery (
  delivery_id integer primary key,
  "date" date,
  cost integer,
  carrier integer references Post(post_id),
  sender integer references Seller(seller_id),
  reciever integer references Customer(customer_id)
);
CREATE TABLE Ord (
  order_id integer primary key,
  "date" date,
  delivery_id integer references Delivery(delivery_id),
  product_id integer references Product(product_id)
);
CREATE TABLE Transaction (
  transcation_id integer primary key,
  "date" date,
  payment_id integer references Payment(payment_id),
  order_id integer references Ord(order_id)
);

CREATE OR REPLACE TRIGGER transaction_payed_fully
before insert 
on Transaction
for each row
begin
if 
(select sum(cost) 
from Product 
left join Ord 
on Ord.product_id = Product.product_id  
where Ord.order_id = :new.order_id) > 
(select sum(amount) 
from Payment 
where payment_id = :new.payment_id) then
raise_application_error(-20000, 'The order is not payed for!');
end if;
end;
/
