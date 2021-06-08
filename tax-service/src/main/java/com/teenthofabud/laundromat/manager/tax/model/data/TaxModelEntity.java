package com.teenthofabud.laundromat.manager.tax.model.data;

import com.teenthofabud.core.common.data.entity.TOABBaseEntity;
import lombok.*;

import javax.persistence.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode
@Entity
@Table(name = "tax_model")
@ToString
public class TaxModelEntity extends TOABBaseEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @ToString.Include
    @EqualsAndHashCode.Include
    private Long id;
    @ToString.Include
    @EqualsAndHashCode.Include
    private String name;
    @ToString.Include
    private String description;
    @ToString.Include
    @EqualsAndHashCode.Include
    @Column(name = "currency_type_model_id")
    private Long currencyTypeModelId;
    @ToString.Include
    @Column(name = "currency_name")
    private String currencyName;
    @ToString.Include
    @EqualsAndHashCode.Include
    private Float rate;
    @ToString.Include
    @EqualsAndHashCode.Include
    @Column(name = "tax_type_model_id")
    private Long taxTypeModelId;

}
