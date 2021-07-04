create table if not exists tax_model (
    id int auto_increment,
    tax_type_model_id int not null,
    currency_type_model_id int not null,
    rate float not null,
    currency_name varchar(100) not null,
    name varchar(100) not null, -- gst-inr, cgst-inr, vat-usd
    description varchar(100),
    created_on timestamp default current_timestamp,
    created_by int not null,
    modified_on timestamp default current_timestamp,
    modified_by int,
    active_sw bigint default 1,
    version int default 0,
    constraint pk_tax_model primary key (id),
    constraint chk_tax_model_rate check (rate > 0)
);
create index if not exists idx_tax_model_tax_type_model_id on tax_model(tax_type_model_id);
create index if not exists idx_tax_model_currency_type_model_id on tax_model(currency_type_model_id);
create index if not exists idx_tax_model_currency_name on tax_model(currency_name);