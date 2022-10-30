create database tourism;
use tourism;

create table package (
uid varchar(8) primary key,
name varchar(30),
mobile float(12),
date date,
boarding_point varchar(50),
destination varchar(50)

);

create table review (
userid varchar(8),
foreign key(userid) references package(uid),
username varchar(30) ,
email varchar(50),
review varchar(500),
stars int,
rb varchar(10)
); 
