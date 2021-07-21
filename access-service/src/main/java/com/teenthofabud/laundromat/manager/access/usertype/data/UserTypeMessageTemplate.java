package com.teenthofabud.laundromat.manager.access.usertype.data;

import lombok.Getter;

@Getter
public enum UserTypeMessageTemplate {

    MSG_TEMPLATE_SEARCHING_FOR_USER_TYPE_ENTITY_ID("Searching for UserTypeEntity with id: {}"),
    MSG_TEMPLATE_NO_USER_TYPE_ENTITY_ID_AVAILABLE("No UserTypeEntity available with id: {}"),
    MSG_TEMPLATE_FOUND_USER_TYPE_ENTITY_ID("Found UserTypeEntity with id: {}"),
    MSG_TEMPLATE_USER_TYPE_ID_VALID("user type id: {} is semantically valid"),
    MSG_TEMPLATE_USER_TYPE_ID_INVALID("user type id: {} is invalid"),
    MSG_TEMPLATE_USER_TYPE_ID_EMPTY("user type id is empty"),
    MSG_TEMPLATE_USER_TYPE_EXISTENCE_BY_NAME ("Checking existence of UserTypeEntity with name: {}"),
    MSG_TEMPLATE_USER_TYPE_EXISTS_BY_NAME ("UserTypeEntity already exists with name: {}"),
    MSG_TEMPLATE_USER_TYPE_NON_EXISTENCE_BY_NAME ("No UserTypeEntity exists with name: {}");

    private String value;

    private UserTypeMessageTemplate(String value) {
        this.value = value;
    }


}
