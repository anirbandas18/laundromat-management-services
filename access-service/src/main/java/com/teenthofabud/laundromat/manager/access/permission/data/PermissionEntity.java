package com.teenthofabud.laundromat.manager.access.permission.data;

import com.teenthofabud.core.common.data.entity.TOABBaseEntity;
import com.teenthofabud.laundromat.manager.access.operation.data.OperationEntity;
import com.teenthofabud.laundromat.manager.access.resource.data.ResourceEntity;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

import javax.persistence.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString
@Entity
@Table(name = "permission_model")
public class PermissionEntity extends TOABBaseEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @ToString.Include
    private Long id;
    /*@EmbeddedId
    private PermissionMap permissionMapping;*/
    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    //@MapsId("resourceId")
    @JoinColumn(name = "resource_model_id")
    private ResourceEntity resource;
    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    //@MapsId("operationId")
    @JoinColumn(name = "operation_model_id")
    private OperationEntity operation;

}
