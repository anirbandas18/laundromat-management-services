package com.teenthofabud.core.common.model.entity;


import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import javax.persistence.Column;
import javax.persistence.MappedSuperclass;
import javax.persistence.Version;
import java.time.LocalDateTime;

@Getter
@Setter
@MappedSuperclass
@ToString
public abstract class TOABBaseEntity {

    @Column(name = "created_on")
    @ToString.Include
    private LocalDateTime createdOn;
    @Column(name = "modified_on")
    @ToString.Include
    private LocalDateTime modifiedOn;
    @Column(name = "created_by")
    @ToString.Include
    private Long createdBy;
    @Column(name = "modified_by")
    @ToString.Include
    private Long modifiedBy;
    @Column(name = "active_sw")
    @ToString.Include
    private Boolean active;
    @ToString.Include
    @Version
    private Integer version;

}
