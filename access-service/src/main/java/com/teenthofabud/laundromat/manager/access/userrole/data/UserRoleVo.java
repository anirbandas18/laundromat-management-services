package com.teenthofabud.laundromat.manager.access.userrole.data;

import com.teenthofabud.core.common.data.vo.TypeModelVo;
import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString
public class UserRoleVo implements Comparable<UserRoleVo> {

    @ToString.Include
    private Long id;
    @ToString.Include
    private Boolean active;
    @ToString.Include
    private TypeModelVo userType;
    @ToString.Include
    private TypeModelVo role;

    @Override
    public int compareTo(UserRoleVo o) {
        return this.id.compareTo(o.getId());
    }
}
