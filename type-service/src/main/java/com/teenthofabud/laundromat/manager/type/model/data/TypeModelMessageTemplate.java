package com.teenthofabud.laundromat.manager.type.model.data;

import lombok.Getter;

@Getter
public enum TypeModelMessageTemplate {

    MSG_TEMPLATE_SEARCHING_FOR_TYPE_MODEL_ENTITY_ID("Searching for TypeModelEntity with id: {}"),
    MSG_TEMPLATE_NO_TYPE_MODEL_ENTITY_ID_AVAILABLE("No TypeModelEntity available with id: {}"),
    MSG_TEMPLATE_FOUND_TYPE_MODEL_ENTITY_ID("Found TypeModelEntity with id: {}"),
    MSG_TEMPLATE_TYPE_MODEL_ID_VALID("type model id: {} is semantically valid"),
    MSG_TEMPLATE_TYPE_MODEL_ID_INVALID("type model id: {} is invalid"),
    MSG_TEMPLATE_TYPE_MODEL_ID_EMPTY("type model id is empty"),
    MSG_TEMPLATE_TYPE_MODEL_EXISTENCE_BY_NAME_AND_TYPE_LOV_ID ("Checking existence of TypeModelEntity with name: {} and typeLovId: {}"),
    MSG_TEMPLATE_TYPE_MODEL_EXISTS_BY_NAME_AND_TYPE_LOV_ID ("TypeModelEntity already exists with name: {} and typeLovId: {}"),
    MSG_TEMPLATE_TYPE_MODEL_NON_EXISTENCE_BY_NAME_AND_TYPE_LOV_ID ("No TypeModelEntity exists with name: {} and typeLovId: {}");

    private String value;

    private TypeModelMessageTemplate(String value) {
        this.value = value;
    }

}
