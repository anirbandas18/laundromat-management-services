package com.teenthofabud.laundromat.manager.access.permission.data;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

@NoArgsConstructor
@AllArgsConstructor
@ToString
@Getter
@Setter
public class PermissionForm {

    @ToString.Include
    private Long resourceId;
    @ToString.Include
    private Long operationId;

}
