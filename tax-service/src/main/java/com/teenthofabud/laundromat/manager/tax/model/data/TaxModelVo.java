package com.teenthofabud.laundromat.manager.tax.model.data;

import com.teenthofabud.core.common.data.vo.TypeModelVo;
import lombok.*;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class TaxModelVo implements Comparable<TaxModelVo> {

    @ToString.Include
    private Long id;
    @ToString.Include
    private Boolean active;
    @ToString.Include
    private String name;
    @ToString.Include
    private String description;
    @ToString.Include
    private Float rate;
    @ToString.Include
    private TypeModelVo currencyTypeModel;
    @ToString.Include
    private TypeModelVo taxTypeModel;

    @Override
    public int compareTo(TaxModelVo o) {
        return Long.compare(this.id, o.getId());
    }

}
