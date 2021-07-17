package com.teenthofabud.laundromat.manager.access.permission.data;

import com.teenthofabud.core.common.data.vo.TypeModelVo;
import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString
public class PermissionVo implements Comparable<PermissionVo> {

    @ToString.Include
    private Long id;
    @ToString.Include
    private Boolean active;
/*
    @ToString.Include
    private Long resourceId;
    @ToString.Include
    private Long operationId;
*/
    @ToString.Include
    private TypeModelVo reource;
    @ToString.Include
    private TypeModelVo operation;

    @Override
    public int compareTo(PermissionVo o) {
        return this.id.compareTo(o.getId());
    }
}
