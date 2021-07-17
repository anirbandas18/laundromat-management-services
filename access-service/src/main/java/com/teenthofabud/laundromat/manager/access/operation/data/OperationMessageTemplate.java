package com.teenthofabud.laundromat.manager.access.operation.data;

import lombok.Getter;

@Getter
public enum OperationMessageTemplate {

    MSG_TEMPLATE_SEARCHING_FOR_OPERATION_ENTITY_ID("Searching for OperationEntity with id: {}"),
    MSG_TEMPLATE_NO_OPERATION_ENTITY_ID_AVAILABLE("No OperationEntity available with id: {}"),
    MSG_TEMPLATE_FOUND_OPERATION_ENTITY_ID("Found OperationEntity with id: {}"),
    MSG_TEMPLATE_OPERATION_ID_VALID("operation id: {} is semantically valid"),
    MSG_TEMPLATE_OPERATION_ID_INVALID("operation id: {} is invalid"),
    MSG_TEMPLATE_OPERATION_ID_EMPTY("operation id is empty"),
    MSG_TEMPLATE_OPERATION_EXISTENCE_BY_NAME ("Checking existence of OperationEntity with name: {}"),
    MSG_TEMPLATE_OPERATION_EXISTS_BY_NAME ("OperationEntity already exists with name: {}"),
    MSG_TEMPLATE_OPERATION_NON_EXISTENCE_BY_NAME ("No OperationEntity exists with name: {}");

    private String value;

    private OperationMessageTemplate(String value) {
        this.value = value;
    }


}
