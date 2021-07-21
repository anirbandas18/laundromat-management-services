package com.teenthofabud.laundromat.manager.access.role.controller;

import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.laundromat.manager.access.error.AccessErrorCode;
import com.teenthofabud.laundromat.manager.access.role.data.RoleException;
import com.teenthofabud.laundromat.manager.access.role.data.RoleForm;
import com.teenthofabud.laundromat.manager.access.role.data.RoleMessageTemplate;
import com.teenthofabud.laundromat.manager.access.role.data.RoleVo;
import com.teenthofabud.laundromat.manager.access.role.service.RoleService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Set;


@RestController
@RequestMapping("role")
@Slf4j
@Tag(name = "Role API", description = "Manage Roles and their details")
public class RoleController {

    private static final String MEDIA_ACCESS_APPLICATION_JSON_PATCH = "application/json-patch+json";

    @Autowired
    public void setService(RoleService service) {
        this.service = service;
    }

    private RoleService service;

    @Operation(summary = "Create new Role details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "Id of newly created Role",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = String.class)) }),
            @ApiResponse(responseCode = "400", description = "Role attribute's value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Role already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Role attributes provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to create new Role",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.CREATED)
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Long> postNewRole(@RequestBody(required = false) RoleForm form) throws RoleException {
        log.debug("Requesting to create new role");
        if(form != null) {
            Long id = service.createRole(form);
            log.debug("Responding with identifier of newly created new role");
            return ResponseEntity.status(HttpStatus.CREATED).body(id);
        }
        log.debug("RoleForm is null");
        throw new RoleException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED,
                new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
    }

    @Operation(summary = "Update Role details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Updated details of Role",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Role attribute's value is invalid/Role is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Role found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Role already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to update Role details",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PutMapping(path = "{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Void> putExistingRole(@PathVariable String id, @RequestBody(required = false) RoleForm form) throws RoleException {
        log.debug("Requesting to update all attributes of existing role");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(RoleMessageTemplate.MSG_TEMPLATE_ROLE_ID_VALID.getValue(), id);
                if(form != null) {
                    service.updateRole(actualId, form);
                    log.debug("Responding with successful updation of attributes for existing role");
                    return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
                }
                log.debug("RoleForm is null");
                throw new RoleException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED,
                        new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
            } catch (NumberFormatException e) {
                log.debug(RoleMessageTemplate.MSG_TEMPLATE_ROLE_ID_INVALID.getValue(), id);
                throw new RoleException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(RoleMessageTemplate.MSG_TEMPLATE_ROLE_ID_EMPTY.getValue());
        throw new RoleException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Soft delete Role by id and all associated Type Models")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Soft deleted Role",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Role id is invalid/Role is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Role found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Role attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to soft delete Role",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @DeleteMapping("{id}")
    public ResponseEntity<Void> deleteExistingRole(@PathVariable String id) throws RoleException {
        log.debug("Requesting to soft delete role");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(RoleMessageTemplate.MSG_TEMPLATE_ROLE_ID_VALID.getValue(), id);
                service.deleteRole(actualId);
                log.debug("Responding with successful deletion of existing role");
                return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
            } catch (NumberFormatException e) {
                log.debug(RoleMessageTemplate.MSG_TEMPLATE_ROLE_ID_INVALID.getValue(), id);
                throw new RoleException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(RoleMessageTemplate.MSG_TEMPLATE_ROLE_ID_EMPTY.getValue());
        throw new RoleException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Patch Role attributes by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Patched each provided attribute of Role with the given value",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Role attribute/value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Role found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Role attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to patch provided attributes of Role with the given values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PatchMapping(path = "{id}", consumes = MEDIA_ACCESS_APPLICATION_JSON_PATCH)
    public ResponseEntity<Void> patchExistingRole(@PathVariable String id, @RequestBody(required = false) List<PatchOperationForm> dtoList)
            throws RoleException {
        log.debug("Requesting to patch of role attributes");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(RoleMessageTemplate.MSG_TEMPLATE_ROLE_ID_VALID.getValue(), id);
                if(dtoList != null) {
                    service.applyPatchOnRole(actualId, dtoList);
                    log.debug("Responding with successful patch of attributes for existing role");
                    return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
                }
                log.debug("role patch document is null");
                throw new RoleException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED,
                        new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
            } catch (NumberFormatException e) {
                log.debug(RoleMessageTemplate.MSG_TEMPLATE_ROLE_ID_INVALID.getValue(), id);
                throw new RoleException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(RoleMessageTemplate.MSG_TEMPLATE_ROLE_ID_EMPTY.getValue());
        throw new RoleException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Get all Role details")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Roles and their details",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = RoleVo.class))) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping
    public Set<RoleVo> getAllRoleNaturallyOrdered() {
        log.debug("Requesting all available roles by their natural orders");
        Set<RoleVo> naturallyOrderedRoles = service.retrieveAllByNaturalOrdering();
        log.debug("Responding with all available roles by their natural orders");
        return naturallyOrderedRoles;
    }

    @Operation(summary = "Get all Role details by name")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Retrieve all available Roles and their details that match the given name",
            content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = RoleVo.class))) }),
        @ApiResponse(responseCode = "400", description = "Role name is invalid",
            content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
        @ApiResponse(responseCode = "404", description = "No Roles available with the given name",
            content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("name/{name}")
    public List<RoleVo> getAllRolesByName(@PathVariable String name) throws RoleException {
        log.debug("Requesting all available roles with given name");
        if(StringUtils.hasText(StringUtils.trimWhitespace(name))) {
            List<RoleVo> matchedByNames = service.retrieveAllMatchingDetailsByName(name);
            log.debug("Responding with all available roles with given name");
            return matchedByNames;
        }
        log.debug("role name is empty");
        throw new RoleException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "name", name });
    }

    @Operation(summary = "Get Role details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve the details of Role that matches the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = RoleVo.class)) }),
            @ApiResponse(responseCode = "400", description = "Role id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Role found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("{id}")
    public RoleVo getRoleDetailsById(@PathVariable String id) throws RoleException {
        log.debug("Requesting all available roles by its id");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(RoleMessageTemplate.MSG_TEMPLATE_ROLE_ID_VALID.getValue(), id);
                RoleVo roleDetails = service.retrieveDetailsById(actualId);
                log.debug("Responding with successful retrieval of existing role details by id");
                return roleDetails;
            } catch (NumberFormatException e) {
                log.debug(RoleMessageTemplate.MSG_TEMPLATE_ROLE_ID_INVALID.getValue(), id);
                throw new RoleException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(RoleMessageTemplate.MSG_TEMPLATE_ROLE_ID_EMPTY.getValue());
        throw new RoleException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

}
