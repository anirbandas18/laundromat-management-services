create table if not exists type_lov (
    id identity not null,
    name varchar2(20) not null,
    description varchar2(100) not null,
    created_on datetime default current_timestamp,
    created_by int default -1,
    modified_on datetime default current_timestamp,
    modified_by int,
    active_sw boolean default true,
    version int default 0,
    constraint pk_type_lov primary key (id)
);
create index if not exists idx_type_lov_name on type_lov(name);

create table if not exists type_model (
    id identity not null,
    type_lov_id int not null,
    name varchar2(20) not null,
    description varchar2(100) not null,
    created_on datetime default current_timestamp,
    created_by int default -1,
    modified_on datetime default current_timestamp,
    modified_by int,
    active_sw boolean default true,
    version int default 0,
    constraint pk_type_model primary key (id),
    constraint fk_type_model_type_lov_id foreign key (type_lov_id) references type_lov(id)
);
create index if not exists idx_type_model_name on type_model(name);
create index if not exists idx_type_model_type_lov_id on type_model(type_lov_id);