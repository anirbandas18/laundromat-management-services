create table if not exists resource_model (
    id identity primary key not null,
    name varchar2(20) not null,
    description varchar2(100),
    created_on datetime default current_timestamp,
    created_by int default -1,
    modified_on datetime default current_timestamp,
    modified_by int,
    active_sw boolean default true,
    version int default 0,
    constraint pk_resource_model primary key (id),
    constraint uq_resource_model_name unique (name)
);
create index if not exists idx_resource_model_id on resource_model(id);
create index if not exists idx_resource_model_name on resource_model(name);

create table if not exists operation_model (
    id identity primary key not null,
    name varchar2(20) not null,
    description varchar2(100),
    created_on datetime default current_timestamp,
    created_by int default -1,
    modified_on datetime default current_timestamp,
    modified_by int,
    active_sw boolean default true,
    version int default 0,
    constraint pk_operation_model primary key (id),
    constraint uq_operation_model_name unique (name)
);
create index if not exists idx_operation_model_id on operation_model(id);
create index if not exists idx_operation_model_name on operation_model(name);

create table if not exists permission_model (
    id identity primary key not null,
    resource_model_id int not null,
    operation_model_id int not null,
    created_on datetime default current_timestamp,
    created_by int default -1,
    modified_on datetime default current_timestamp,
    modified_by int,
    active_sw boolean default true,
    version int default 0,
    constraint pk_permission_model primary key (id),
    constraint fk_permission_model_resource_model_id foreign key (resource_model_id) references resource_model(id),
    constraint fk_permission_model_operation_model_id foreign key (operation_model_id) references operation_model(id),
    constraint uq_permission_model_resource_model_id_operation_model_id unique (resource_model_id, operation_model_id)
);
create index if not exists idx_permission_model_resource_model_id on permission_model(resource_model_id);
create index if not exists idx_permission_model_operation_model_id on permission_model(operation_model_id);