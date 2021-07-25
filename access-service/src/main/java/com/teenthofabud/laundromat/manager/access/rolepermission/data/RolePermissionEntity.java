package com.teenthofabud.laundromat.manager.access.rolepermission.data;

import com.teenthofabud.core.common.data.entity.TOABBaseEntity;
import com.teenthofabud.laundromat.manager.access.permission.data.PermissionEntity;
import com.teenthofabud.laundromat.manager.access.role.data.RoleEntity;
import lombok.*;

import javax.persistence.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString
@Entity
@Table(name = "role_permission_model")
public class RolePermissionEntity extends TOABBaseEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @ToString.Include
    private Long id;
    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "role_lov_id")
    private RoleEntity role;
    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "permission_model_id")
    private PermissionEntity permission;

}
