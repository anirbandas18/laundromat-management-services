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

create table if not exists user_model (
    id identity primary key not null,
    name varchar2(20) not null,
    description varchar2(100),
    created_on datetime default current_timestamp,
    created_by int default -1,
    modified_on datetime default current_timestamp,
    modified_by int,
    active_sw boolean default true,
    version int default 0,
    constraint pk_user_model primary key (id),
    constraint uq_user_model_name unique (name)
);
create index if not exists idx_user_model_id on user_model(id);
create index if not exists idx_user_model_name on user_model(name);

create table if not exists role_model (
    id identity primary key not null,
    name varchar2(20) not null,
    description varchar2(100),
    created_on datetime default current_timestamp,
    created_by int default -1,
    modified_on datetime default current_timestamp,
    modified_by int,
    active_sw boolean default true,
    version int default 0,
    constraint pk_role_model primary key (id),
    constraint uq_role_model_name unique (name)
);
create index if not exists idx_role_model_id on role_model(id);
create index if not exists idx_role_model_name on role_model(name);

create table if not exists user_role (
    id identity primary key not null,
    user_model_id int not null,
    role_model_id int not null,
    created_on datetime default current_timestamp,
    created_by int default -1,
    modified_on datetime default current_timestamp,
    modified_by int,
    active_sw boolean default true,
    version int default 0,
    constraint pk_user_role primary key (id),
    constraint fk_user_role_user_model_id foreign key (user_model_id) references user_model(id),
    constraint fk_user_role_role_model_id foreign key (role_model_id) references role_model(id),
    constraint uq_user_role_user_model_id_role_model_id unique (user_model_id, role_model_id)
);
create index if not exists idx_user_role_user_model_id on user_role(user_model_id);
create index if not exists idx_user_role_role_model_id on user_role(role_model_id);

create table if not exists role_permission (
    id identity primary key not null,
    role_model_id int not null,
    permission_model_id int not null,
    created_on datetime default current_timestamp,
    created_by int default -1,
    modified_on datetime default current_timestamp,
    modified_by int,
    active_sw boolean default true,
    version int default 0,
    constraint pk_role_permission primary key (id),
    constraint fk_role_permission_role_model_id foreign key (role_model_id) references role_model(id),
    constraint fk_role_permission_permission_model_id foreign key (permission_model_id) references permission_model(id),
    constraint uq_role_permission_role_model_id_permission_model_id unique (role_model_id, permission_model_id)
);
create index if not exists idx_role_permission_role_model_id on role_permission(role_model_id);
create index if not exists idx_role_permission_permission_model_id on role_permission(permission_model_id);
