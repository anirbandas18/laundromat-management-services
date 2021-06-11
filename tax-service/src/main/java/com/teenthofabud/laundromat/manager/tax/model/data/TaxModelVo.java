package com.teenthofabud.laundromat.manager.tax.model.data;

import lombok.*;

@Getter
@Setter
@AllArgsConstructor
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

    @Getter
    @Setter
    @AllArgsConstructor
    public static final class TypeModelVo {
        private Long id;
        private String name;
        public TypeModelVo() {
            this.id = 0L;
            this.name = "";
        }
    }

    public TaxModelVo() {
        this.id = 0L;
        this.active = null;
        this.name = "";
        this.description = "";
        this.rate = 0.0F;
        this.taxTypeModel = new TypeModelVo();
        this.currencyTypeModel = new TypeModelVo();
    }

}
