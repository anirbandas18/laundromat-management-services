package com.teenthofabud.laundromat.manager.type.lov.data;

import com.teenthofabud.core.common.model.entity.TOABBaseEntity;
import com.teenthofabud.laundromat.manager.type.model.data.TypeModelEntity;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

import javax.persistence.*;
import java.util.Set;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "type_lov")
@ToString
public class TypeLOVEntity extends TOABBaseEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @ToString.Include
    private Long id;
    @ToString.Include
    private String name;
    @ToString.Include
    private String description;
    @ToString.Exclude
    @OneToMany(mappedBy = "typeLov", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<TypeModelEntity> modelEntities;

    public void addTypeModelEntity(TypeModelEntity typeModelEntity) {
        if(this.modelEntities != null) {
            this.modelEntities.add(typeModelEntity);
        }
    }

    public void removeTypeModelEntity(TypeModelEntity typeModelEntity) {
        if(this.modelEntities != null) {
            this.modelEntities.remove(typeModelEntity);
        }
    }

}
