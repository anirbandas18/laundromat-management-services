package com.teenthofabud.laundromat.manager.tax.lov.data;

import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString
public class TaxLOVVo implements Comparable<TaxLOVVo> {

    @ToString.Include
    private Long id;
    @ToString.Include
    private Boolean active;
    @ToString.Include
    private String name;
    @ToString.Include
    private String description;

    @Override
    public int compareTo(TaxLOVVo o) {
        return Long.compare(this.id, o.getId());
    }
}
