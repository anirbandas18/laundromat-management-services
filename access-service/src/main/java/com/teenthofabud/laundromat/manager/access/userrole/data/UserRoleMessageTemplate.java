package com.teenthofabud.laundromat.manager.access.userrole.data;

import lombok.Getter;

@Getter
public enum UserRoleMessageTemplate {

    MSG_TEMPLATE_SEARCHING_FOR_USER_ROLE_ENTITY_ID("Searching for UserRoleEntity with id: {}"),
    MSG_TEMPLATE_NO_USER_ROLE_ENTITY_ID_AVAILABLE("No UserRoleEntity available with id: {}"),
    MSG_TEMPLATE_FOUND_USER_ROLE_ENTITY_ID("Found UserRoleEntity with id: {}"),
    MSG_TEMPLATE_USER_ROLE_ID_VALID("user role id: {} is semantically valid"),
    MSG_TEMPLATE_USER_ROLE_ID_INVALID("user role id: {} is invalid"),
    MSG_TEMPLATE_USER_ROLE_ID_EMPTY("user role id is empty"),
    MSG_TEMPLATE_USER_ROLE_EXISTENCE_BY_USER_TYPE_ID ("Checking existence of UserRoleEntity with user type id: {}"),
    MSG_TEMPLATE_USER_ROLE_EXISTS_BY_USER_TYPE_ID ("UserRoleEntity already exists with user type id: {}"),
    MSG_TEMPLATE_USER_ROLE_NON_EXISTENCE_BY_USER_TYPE_ID ("No UserRoleEntity exists with user type id: {}"),
    MSG_TEMPLATE_USER_ROLE_EXISTENCE_BY_ROLE_ID ("Checking existence of UserRoleEntity with role id: {}"),
    MSG_TEMPLATE_USER_ROLE_EXISTS_BY_ROLE_ID ("UserRoleEntity already exists with role id: {}"),
    MSG_TEMPLATE_USER_ROLE_NON_EXISTENCE_BY_ROLE_ID ("No UserRoleEntity exists with role id: {}"),
    MSG_TEMPLATE_USER_ROLE_EXISTENCE_BY_USER_TYPE_ID_ROLE_ID ("Checking existence of UserRoleEntity with user type id: {} and role id: {}"),
    MSG_TEMPLATE_USER_ROLE_EXISTS_BY_USER_TYPE_ID_ROLE_ID ("UserRoleEntity already exists with user type id: {} and role id: {}"),
    MSG_TEMPLATE_USER_ROLE_NON_EXISTENCE_BY_USER_TYPE_ID_ROLE_ID ("No UserRoleEntity exists with user type id: {} and role id: {}"),
    MSG_TEMPLATE_USER_ROLE_DTO_USER_TYPE_ID_INVALID("UserRoleDto.userTypeId is invalid"),
    MSG_TEMPLATE_USER_ROLE_DTO_USER_TYPE_ID_INACTIVE("UserRoleDto.userTypeId is inactive"),
    MSG_TEMPLATE_USER_ROLE_DTO_ROLE_ID_INVALID("UserRoleDto.roleId is invalid"),
    MSG_TEMPLATE_USER_ROLE_DTO_ROLE_ID_INACTIVE("UserRoleDto.roleId is inactive");


    private String value;

    private UserRoleMessageTemplate(String value) {
        this.value = value;
    }


}
