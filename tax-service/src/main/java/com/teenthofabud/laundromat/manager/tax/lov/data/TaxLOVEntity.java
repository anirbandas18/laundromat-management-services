package com.teenthofabud.laundromat.manager.tax.lov.data;

import com.teenthofabud.core.common.data.entity.TOABBaseEntity;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelEntity;
import lombok.*;

import javax.persistence.*;
import java.util.Set;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "tax_lov")
@ToString
public class TaxLOVEntity extends TOABBaseEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @ToString.Include
    private Long id;
    @ToString.Include
    private String name;
    @ToString.Include
    private String description;
    @ToString.Exclude
    @OneToMany(mappedBy = "taxLov", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<TaxModelEntity> modelEntities;

    public void addTaxModelEntity(TaxModelEntity taxModelEntity) {
        if(this.modelEntities != null) {
            this.modelEntities.add(taxModelEntity);
        }
    }

    public void removeTaxModelEntity(TaxModelEntity taxModelEntity) {
        if(this.modelEntities != null) {
            this.modelEntities.remove(taxModelEntity);
        }
    }

}
