package com.teenthofabud.laundromat.manager.tax.model.data;

import com.teenthofabud.core.common.data.entity.TOABBaseEntity;
import com.teenthofabud.core.common.data.entity.TypeModelEntity;
import com.teenthofabud.laundromat.manager.tax.lov.data.TaxLOVEntity;
import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

import javax.persistence.AttributeOverride;
import javax.persistence.AttributeOverrides;
import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

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
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "tax_lov_id")
    private TaxLOVEntity taxLov;

}
