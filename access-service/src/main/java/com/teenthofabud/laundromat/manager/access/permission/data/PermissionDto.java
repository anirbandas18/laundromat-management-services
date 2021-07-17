package com.teenthofabud.laundromat.manager.access.permission.data;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.util.Optional;

@Getter
@Setter
@ToString
@AllArgsConstructor
public class PermissionDto {

    @ToString.Include
    private Optional<String> resourceId;
    @ToString.Include
    private Optional<String> operationId;
    @ToString.Include
    private Optional<String> active;


    public PermissionDto() {
        this.resourceId = Optional.ofNullable(null);
        this.operationId = Optional.ofNullable(null);
        this.active = Optional.ofNullable(null);
    }

}
