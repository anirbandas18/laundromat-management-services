package com.teenthofabud.laundromat.manager.access.userrole.data;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.util.Optional;

@Getter
@Setter
@ToString
@AllArgsConstructor
public class UserRoleDto {

    @ToString.Include
    private Optional<String> userTypeId;
    @ToString.Include
    private Optional<String> roleId;
    @ToString.Include
    private Optional<String> active;


    public UserRoleDto() {
        this.userTypeId = Optional.ofNullable(null);
        this.roleId = Optional.ofNullable(null);
        this.active = Optional.ofNullable(null);
    }

}
