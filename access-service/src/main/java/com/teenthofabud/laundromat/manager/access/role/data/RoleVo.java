package com.teenthofabud.laundromat.manager.access.role.data;

import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString
public class RoleVo implements Comparable<RoleVo> {

    @ToString.Include
    private Long id;
    @ToString.Include
    private Boolean active;
    @ToString.Include
    private String name;
    @ToString.Include
    private String description;

    @Override
    public int compareTo(RoleVo o) {
        return this.id.compareTo(o.getId());
    }
}
