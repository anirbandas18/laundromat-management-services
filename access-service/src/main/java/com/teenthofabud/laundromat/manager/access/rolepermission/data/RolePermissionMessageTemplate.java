package com.teenthofabud.laundromat.manager.access.rolepermission.data;

import lombok.Getter;

@Getter
public enum RolePermissionMessageTemplate {

    MSG_TEMPLATE_SEARCHING_FOR_ROLE_PERMISSION_ENTITY_ID("Searching for RolePermissionEntity with id: {}"),
    MSG_TEMPLATE_NO_ROLE_PERMISSION_ENTITY_ID_AVAILABLE("No RolePermissionEntity available with id: {}"),
    MSG_TEMPLATE_FOUND_ROLE_PERMISSION_ENTITY_ID("Found RolePermissionEntity with id: {}"),
    MSG_TEMPLATE_ROLE_PERMISSION_ID_VALID("role permission id: {} is semantically valid"),
    MSG_TEMPLATE_ROLE_PERMISSION_ID_INVALID("role permission id: {} is invalid"),
    MSG_TEMPLATE_ROLE_PERMISSION_ID_EMPTY("role permission id is empty"),
    MSG_TEMPLATE_ROLE_PERMISSION_EXISTENCE_BY_PERMISSION_ID ("Checking existence of RolePermissionEntity with permission id: {}"),
    MSG_TEMPLATE_ROLE_PERMISSION_EXISTS_BY_PERMISSION_ID ("RolePermissionEntity already exists with permission id: {}"),
    MSG_TEMPLATE_ROLE_PERMISSION_NON_EXISTENCE_BY_PERMISSION_ID ("No RolePermissionEntity exists with permission id: {}"),
    MSG_TEMPLATE_ROLE_PERMISSION_EXISTENCE_BY_ROLE_ID ("Checking existence of RolePermissionEntity with role id: {}"),
    MSG_TEMPLATE_ROLE_PERMISSION_EXISTS_BY_ROLE_ID ("RolePermissionEntity already exists with role id: {}"),
    MSG_TEMPLATE_ROLE_PERMISSION_NON_EXISTENCE_BY_ROLE_ID ("No RolePermissionEntity exists with role id: {}"),
    MSG_TEMPLATE_ROLE_PERMISSION_EXISTENCE_BY_PERMISSION_ID_ROLE_ID ("Checking existence of RolePermissionEntity with permission id: {} and role id: {}"),
    MSG_TEMPLATE_ROLE_PERMISSION_EXISTS_BY_PERMISSION_ID_ROLE_ID ("RolePermissionEntity already exists with permission id: {} and role id: {}"),
    MSG_TEMPLATE_ROLE_PERMISSION_NON_EXISTENCE_BY_PERMISSION_ID_ROLE_ID ("No RolePermissionEntity exists with permission id: {} and role id: {}"),
    MSG_TEMPLATE_ROLE_PERMISSION_DTO_PERMISSION_ID_INVALID("RolePermissionDto.permissionId is invalid"),
    MSG_TEMPLATE_ROLE_PERMISSION_DTO_PERMISSION_ID_INACTIVE("RolePermissionDto.permissionId is inactive"),
    MSG_TEMPLATE_ROLE_PERMISSION_DTO_ROLE_ID_INVALID("RolePermissionDto.roleId is invalid"),
    MSG_TEMPLATE_ROLE_PERMISSION_DTO_ROLE_ID_INACTIVE("RolePermissionDto.roleId is inactive");


    private String value;

    private RolePermissionMessageTemplate(String value) {
        this.value = value;
    }


}
