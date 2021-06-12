package com.teenthofabud.laundromat.manager.tax.model.data;

import com.teenthofabud.core.common.data.entity.TOABBaseEntity;
import com.teenthofabud.core.common.data.entity.TypeModelEntity;
import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

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
    @Embedded
    @AttributeOverrides({
            @AttributeOverride( name = "id", column = @Column(name = "currency_type_model_id")),
            @AttributeOverride( name = "name", column = @Column(name = "currency_name"))
    })
    private TypeModelEntity currencyTypeModel;
    @ToString.Include
    @EqualsAndHashCode.Include
    private Float rate;
    @ToString.Include
    @EqualsAndHashCode.Include
    @Column(name = "tax_type_model_id")
    private Long taxTypeModelId;

}
