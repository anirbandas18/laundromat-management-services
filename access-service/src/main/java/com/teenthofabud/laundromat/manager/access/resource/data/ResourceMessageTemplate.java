package com.teenthofabud.laundromat.manager.access.resource.data;

import lombok.Getter;

@Getter
public enum ResourceMessageTemplate {

    MSG_TEMPLATE_SEARCHING_FOR_RESOURCE_ENTITY_ID("Searching for ResourceEntity with id: {}"),
    MSG_TEMPLATE_NO_RESOURCE_ENTITY_ID_AVAILABLE("No ResourceEntity available with id: {}"),
    MSG_TEMPLATE_FOUND_RESOURCE_ENTITY_ID("Found ResourceEntity with id: {}"),
    MSG_TEMPLATE_RESOURCE_ID_VALID("resource id: {} is semantically valid"),
    MSG_TEMPLATE_RESOURCE_ID_INVALID("resource id: {} is invalid"),
    MSG_TEMPLATE_RESOURCE_ID_EMPTY("resource id is empty"),
    MSG_TEMPLATE_RESOURCE_EXISTENCE_BY_NAME ("Checking existence of ResourceEntity with name: {}"),
    MSG_TEMPLATE_RESOURCE_EXISTS_BY_NAME ("ResourceEntity already exists with name: {}"),
    MSG_TEMPLATE_RESOURCE_NON_EXISTENCE_BY_NAME ("No ResourceEntity exists with name: {}");

    private String value;

    private ResourceMessageTemplate(String value) {
        this.value = value;
    }


}
