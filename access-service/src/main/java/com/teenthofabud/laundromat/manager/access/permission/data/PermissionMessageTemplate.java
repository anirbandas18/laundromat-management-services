package com.teenthofabud.laundromat.manager.access.permission.data;

import lombok.Getter;

@Getter
public enum PermissionMessageTemplate {

    MSG_TEMPLATE_SEARCHING_FOR_PERMISSION_ENTITY_ID("Searching for PermissionEntity with id: {}"),
    MSG_TEMPLATE_NO_PERMISSION_ENTITY_ID_AVAILABLE("No PermissionEntity available with id: {}"),
    MSG_TEMPLATE_FOUND_PERMISSION_ENTITY_ID("Found PermissionEntity with id: {}"),
    MSG_TEMPLATE_PERMISSION_ID_VALID("permission id: {} is semantically valid"),
    MSG_TEMPLATE_PERMISSION_ID_INVALID("permission id: {} is invalid"),
    MSG_TEMPLATE_PERMISSION_ID_EMPTY("permission id is empty"),
    MSG_TEMPLATE_PERMISSION_EXISTENCE_BY_RESOURCE_ID ("Checking existence of PermissionEntity with resource id: {}"),
    MSG_TEMPLATE_PERMISSION_EXISTS_BY_RESOURCE_ID ("PermissionEntity already exists with resource id: {}"),
    MSG_TEMPLATE_PERMISSION_NON_EXISTENCE_BY_RESOURCE_ID ("No PermissionEntity exists with resource id: {}"),
    MSG_TEMPLATE_PERMISSION_EXISTENCE_BY_OPERATION_ID ("Checking existence of PermissionEntity with operation id: {}"),
    MSG_TEMPLATE_PERMISSION_EXISTS_BY_OPERATION_ID ("PermissionEntity already exists with operation id: {}"),
    MSG_TEMPLATE_PERMISSION_NON_EXISTENCE_BY_OPERATION_ID ("No PermissionEntity exists with operation id: {}"),
    MSG_TEMPLATE_PERMISSION_EXISTENCE_BY_RESOURCE_ID_OPERATION_ID ("Checking existence of PermissionEntity with resource id: {} and operation id: {}"),
    MSG_TEMPLATE_PERMISSION_EXISTS_BY_RESOURCE_ID_OPERATION_ID ("PermissionEntity already exists with resource id: {} and operation id: {}"),
    MSG_TEMPLATE_PERMISSION_NON_EXISTENCE_BY_RESOURCE_ID_OPERATION_ID ("No PermissionEntity exists with resource id: {} and operation id: {}"),
    MSG_TEMPLATE_PERMISSION_DTO_RESOURCE_ID_INVALID("PermissionDto.resourceId is invalid"),
    MSG_TEMPLATE_PERMISSION_DTO_RESOURCE_ID_INACTIVE("PermissionDto.resourceId is inactive"),
    MSG_TEMPLATE_PERMISSION_DTO_OPERATION_ID_INVALID("PermissionDto.operationId is invalid"),
    MSG_TEMPLATE_PERMISSION_DTO_OPERATION_ID_INACTIVE("PermissionDto.operationId is inactive");


    private String value;

    private PermissionMessageTemplate(String value) {
        this.value = value;
    }


}
