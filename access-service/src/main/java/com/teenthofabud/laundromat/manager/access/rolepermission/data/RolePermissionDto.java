package com.teenthofabud.laundromat.manager.access.rolepermission.data;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.util.Optional;

@Getter
@Setter
@ToString
@AllArgsConstructor
public class RolePermissionDto {

    @ToString.Include
    private Optional<String> permissionId;
    @ToString.Include
    private Optional<String> roleId;
    @ToString.Include
    private Optional<String> active;


    public RolePermissionDto() {
        this.permissionId = Optional.ofNullable(null);
        this.roleId = Optional.ofNullable(null);
        this.active = Optional.ofNullable(null);
    }

}
