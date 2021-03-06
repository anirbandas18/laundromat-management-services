package com.teenthofabud.laundromat.manager.type.lov.data;

import lombok.Getter;

@Getter
public enum TypeLOVMessageTemplate {

    MSG_TEMPLATE_SEARCHING_FOR_TYPE_LOV_ENTITY_ID("Searching for TypeLOVEntity with id: {}"),
    MSG_TEMPLATE_NO_TYPE_LOV_ENTITY_ID_AVAILABLE("No TypeLOVEntity available with id: {}"),
    MSG_TEMPLATE_FOUND_TYPE_LOV_ENTITY_ID("Found TypeLOVEntity with id: {}"),
    MSG_TEMPLATE_TYPE_LOV_ID_VALID("type LOV id: {} is semantically valid"),
    MSG_TEMPLATE_TYPE_LOV_ID_INVALID("type LOV id: {} is invalid"),
    MSG_TEMPLATE_TYPE_LOV_ID_EMPTY("type LOV id is empty"),
    MSG_TEMPLATE_TYPE_LOV_EXISTENCE_BY_NAME ("Checking existence of TypeLOVEntity with name: {}"),
    MSG_TEMPLATE_TYPE_LOV_EXISTS_BY_NAME ("TypeLOVEntity already exists with name: {}"),
    MSG_TEMPLATE_TYPE_LOV_NON_EXISTENCE_BY_NAME ("No TypeLOVEntity exists with name: {}");

    private String value;

    private TypeLOVMessageTemplate(String value) {
        this.value = value;
    }

}
