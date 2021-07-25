create table if not exists resource_lov (
    id identity not null,
    name varchar2(20) not null,
    description varchar2(100),
    created_on datetime default current_timestamp,
    created_by int default -1,
    modified_on datetime default current_timestamp,
    modified_by int,
    active_sw boolean default true,
    version int default 0,
    constraint pk_resource_lov primary key (id),
    constraint uq_resource_lov_name unique (name)
);
create index if not exists idx_resource_lov_id on resource_lov(id);
create index if not exists idx_resource_lov_name on resource_lov(name);

create table if not exists operation_lov (
    id identity not null,
    name varchar2(20) not null,
    description varchar2(100),
    created_on datetime default current_timestamp,
    created_by int default -1,
    modified_on datetime default current_timestamp,
    modified_by int,
    active_sw boolean default true,
    version int default 0,
    constraint pk_operation_lov primary key (id),
    constraint uq_operation_lov_name unique (name)
);
create index if not exists idx_operation_lov_id on operation_lov(id);
create index if not exists idx_operation_lov_name on operation_lov(name);

create table if not exists permission_model (
    id identity not null,
    resource_lov_id int not null,
    operation_lov_id int not null,
    created_on datetime default current_timestamp,
    created_by int default -1,
    modified_on datetime default current_timestamp,
    modified_by int,
    active_sw boolean default true,
    version int default 0,
    constraint pk_permission_model primary key (id),
    constraint fk_permission_model_resource_model_id foreign key (resource_lov_id) references resource_lov(id),
    constraint fk_permission_model_operation_model_id foreign key (operation_lov_id) references operation_lov(id),
    constraint uq_permission_model_resource_model_id_operation_model_id unique (resource_lov_id, operation_lov_id)
);
create index if not exists idx_permission_model_resource_lov_id on permission_model(resource_lov_id);
create index if not exists idx_permission_model_operation_lov_id on permission_model(operation_lov_id);

create table if not exists user_lov (
    id identity not null,
    name varchar2(20) not null,
    description varchar2(100),
    created_on datetime default current_timestamp,
    created_by int default -1,
    modified_on datetime default current_timestamp,
    modified_by int,
    active_sw boolean default true,
    version int default 0,
    constraint pk_user_lov primary key (id),
    constraint uq_user_lov_name unique (name)
);
create index if not exists idx_user_lov_id on user_lov(id);
create index if not exists idx_user_lov_name on user_lov(name);

create table if not exists role_lov (
    id identity not null,
    name varchar2(20) not null,
    description varchar2(100),
    created_on datetime default current_timestamp,
    created_by int default -1,
    modified_on datetime default current_timestamp,
    modified_by int,
    active_sw boolean default true,
    version int default 0,
    constraint pk_role_lov primary key (id),
    constraint uq_role_lov_name unique (name)
);
create index if not exists idx_role_lov_id on role_lov(id);
create index if not exists idx_role_lov_name on role_lov(name);

create table if not exists user_role_model (
    id identity not null,
    user_lov_id int not null,
    role_lov_id int not null,
    created_on datetime default current_timestamp,
    created_by int default -1,
    modified_on datetime default current_timestamp,
    modified_by int,
    active_sw boolean default true,
    version int default 0,
    constraint pk_user_role_model primary key (id),
    constraint fk_user_role_model_user_lov_id foreign key (user_lov_id) references user_lov(id),
    constraint fk_user_role_model_role_lov_id foreign key (role_lov_id) references role_lov(id),
    constraint uq_user_role_model_user_lov_id_role_lov_id unique (user_lov_id, role_lov_id)
);
create index if not exists idx_user_role_model_user_lov_id on user_role_model(user_lov_id);
create index if not exists idx_user_role_model_role_lov_id on user_role_model(role_lov_id);

create table if not exists role_permission_model (
    id identity not null,
    role_lov_id int not null,
    permission_model_id int not null,
    created_on datetime default current_timestamp,
    created_by int default -1,
    modified_on datetime default current_timestamp,
    modified_by int,
    active_sw boolean default true,
    version int default 0,
    constraint pk_role_permission_model primary key (id),
    constraint fk_role_permission_model_role_lov_id foreign key (role_lov_id) references role_lov(id),
    constraint fk_role_permission_model_permission_model_id foreign key (permission_model_id) references permission_model(id),
    constraint uq_role_permission_model_role_lov_id_permission_model_id unique (role_lov_id, permission_model_id)
);
create index if not exists idx_role_permission_model_role_model_id on role_permission_model(role_lov_id);
create index if not exists idx_role_permission_model_permission_model_id on role_permission_model(permission_model_id);

create table if not exists security_question_lov (
    id identity not null,
    name varchar(255) not null,
    created_on timestamp default current_timestamp,
    created_by int not null,
    modified_on timestamp default current_timestamp,
    modified_by int,
    active_sw bigint default 1,
    version int default 0,
    constraint pk_security_question_lov primary key (id),
    constraint uq_security_question_lov_name unique (name)
);
create index if not exists idx_security_question_lov_id on security_question_lov(id);
create index if not exists idx_security_question_lov_name on security_question_lov(name);