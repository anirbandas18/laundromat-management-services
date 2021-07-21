package com.teenthofabud.laundromat.manager.access.role.data;

import lombok.Getter;

@Getter
public enum RoleMessageTemplate {

    MSG_TEMPLATE_SEARCHING_FOR_ROLE_ENTITY_ID("Searching for RoleEntity with id: {}"),
    MSG_TEMPLATE_NO_ROLE_ENTITY_ID_AVAILABLE("No RoleEntity available with id: {}"),
    MSG_TEMPLATE_FOUND_ROLE_ENTITY_ID("Found RoleEntity with id: {}"),
    MSG_TEMPLATE_ROLE_ID_VALID("role id: {} is semantically valid"),
    MSG_TEMPLATE_ROLE_ID_INVALID("role id: {} is invalid"),
    MSG_TEMPLATE_ROLE_ID_EMPTY("role id is empty"),
    MSG_TEMPLATE_ROLE_EXISTENCE_BY_NAME ("Checking existence of RoleEntity with name: {}"),
    MSG_TEMPLATE_ROLE_EXISTS_BY_NAME ("RoleEntity already exists with name: {}"),
    MSG_TEMPLATE_ROLE_NON_EXISTENCE_BY_NAME ("No RoleEntity exists with name: {}");

    private String value;

    private RoleMessageTemplate(String value) {
        this.value = value;
    }


}
