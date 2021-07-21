package com.teenthofabud.laundromat.manager.access.rolepermission.data;

import com.teenthofabud.core.common.data.vo.TypeModelVo;
import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString
public class RolePermissionVo implements Comparable<RolePermissionVo> {

    @ToString.Include
    private Long id;
    @ToString.Include
    private Boolean active;
    @ToString.Include
    private Long permissionId;
    @ToString.Include
    private Long roleId;

    @Override
    public int compareTo(RolePermissionVo o) {
        return this.id.compareTo(o.getId());
    }
}
