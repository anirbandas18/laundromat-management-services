package com.teenthofabud.laundromat.manager.type.model.entity;

import com.teenthofabud.core.common.model.entity.TOABBaseEntity;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.Table;
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
