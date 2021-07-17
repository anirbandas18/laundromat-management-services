package com.teenthofabud.laundromat.manager.access.operation.data;

import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString
public class OperationVo implements Comparable<OperationVo> {

    @ToString.Include
    private Long id;
    @ToString.Include
    private Boolean active;
    @ToString.Include
    private String name;
    @ToString.Include
    private String description;

    @Override
    public int compareTo(OperationVo o) {
        return this.id.compareTo(o.getId());
    }
}
