package com.teenthofabud.laundromat.manager.access.resource.data;

import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString
public class ResourceVo implements Comparable<ResourceVo> {

    @ToString.Include
    private Long id;
    @ToString.Include
    private Boolean active;
    @ToString.Include
    private String name;
    @ToString.Include
    private String description;

    @Override
    public int compareTo(ResourceVo o) {
        return this.id.compareTo(o.getId());
    }
}
