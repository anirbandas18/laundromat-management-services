create table if not exists tax_lov (
    id identity,
    name varchar(20) not null, -- discount, cashback
    description varchar(100),
    created_on timestamp default current_timestamp,
    created_by int not null,
    modified_on timestamp default current_timestamp,
    modified_by int,
    active_sw bigint default 1,
    version int default 0,
    constraint pk_tax_lov primary key (id),
    constraint uq_tax_lov_name unique (name)
);
create index if not exists idx_tax_lov_id on tax_lov(id);
create index if not exists idx_tax_lov_name on tax_lov(name);


create table if not exists tax_model (
    id identity,
    tax_lov_id int not null,
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
    constraint fk_tax_model_tax_lov_id foreign key (tax_lov_id) references tax_lov(id),
    constraint uq_tax_model_id_tax_lov_id_currency_type_model_id unique (id, tax_lov_id, currency_type_model_id),
    constraint chk_tax_model_rate check (rate > 0)
);
create index if not exists idx_tax_model_tax_lov_id on tax_model(tax_lov_id);
create index if not exists idx_tax_model_currency_type_model_id on tax_model(currency_type_model_id);
create index if not exists idx_tax_model_currency_name on tax_model(currency_name);