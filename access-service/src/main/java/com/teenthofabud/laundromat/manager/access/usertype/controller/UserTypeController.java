package com.teenthofabud.laundromat.manager.access.usertype.controller;

import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.laundromat.manager.access.error.AccessErrorCode;
import com.teenthofabud.laundromat.manager.access.usertype.data.UserTypeException;
import com.teenthofabud.laundromat.manager.access.usertype.data.UserTypeForm;
import com.teenthofabud.laundromat.manager.access.usertype.data.UserTypeMessageTemplate;
import com.teenthofabud.laundromat.manager.access.usertype.data.UserTypeVo;
import com.teenthofabud.laundromat.manager.access.usertype.service.UserTypeService;
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
@RequestMapping("usertype")
@Slf4j
@Tag(name = "UserType API", description = "Manage UserTypes and their details")
public class UserTypeController {

    private static final String MEDIA_ACCESS_APPLICATION_JSON_PATCH = "application/json-patch+json";

    @Autowired
    public void setService(UserTypeService service) {
        this.service = service;
    }

    private UserTypeService service;

    @Operation(summary = "Create new UserType details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "Id of newly created Operation",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = String.class)) }),
            @ApiResponse(responseCode = "400", description = "UserType attribute's value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "UserType already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No UserType attributes provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to create new Operation",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.CREATED)
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Long> postNewOperation(@RequestBody(required = false) UserTypeForm form) throws UserTypeException {
        log.debug("Requesting to create new userType");
        if(form != null) {
            Long id = service.createUserType(form);
            log.debug("Responding with identifier of newly created new userType");
            return ResponseEntity.status(HttpStatus.CREATED).body(id);
        }
        log.debug("OperationForm is null");
        throw new UserTypeException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED,
                new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
    }

    @Operation(summary = "Update UserType details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Updated details of Operation",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "UserType attribute's value is invalid/UserType is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No UserType found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "UserType already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to update UserType details",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PutMapping(path = "{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Void> putExistingOperation(@PathVariable String id, @RequestBody(required = false) UserTypeForm form) throws UserTypeException {
        log.debug("Requesting to update all attributes of existing userType");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(UserTypeMessageTemplate.MSG_TEMPLATE_USER_TYPE_ID_VALID.getValue(), id);
                if(form != null) {
                    service.updateUserType(actualId, form);
                    log.debug("Responding with successful updation of attributes for existing userType");
                    return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
                }
                log.debug("OperationForm is null");
                throw new UserTypeException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED,
                        new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
            } catch (NumberFormatException e) {
                log.debug(UserTypeMessageTemplate.MSG_TEMPLATE_USER_TYPE_ID_INVALID.getValue(), id);
                throw new UserTypeException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(UserTypeMessageTemplate.MSG_TEMPLATE_USER_TYPE_ID_EMPTY.getValue());
        throw new UserTypeException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Soft delete UserType by id and all associated Type Models")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Soft deleted Operation",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "UserType id is invalid/UserType is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No UserType found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No UserType attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to soft delete Operation",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @DeleteMapping("{id}")
    public ResponseEntity<Void> deleteExistingOperation(@PathVariable String id) throws UserTypeException {
        log.debug("Requesting to soft delete userType");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(UserTypeMessageTemplate.MSG_TEMPLATE_USER_TYPE_ID_VALID.getValue(), id);
                service.deleteUserType(actualId);
                log.debug("Responding with successful deletion of existing userType");
                return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
            } catch (NumberFormatException e) {
                log.debug(UserTypeMessageTemplate.MSG_TEMPLATE_USER_TYPE_ID_INVALID.getValue(), id);
                throw new UserTypeException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(UserTypeMessageTemplate.MSG_TEMPLATE_USER_TYPE_ID_EMPTY.getValue());
        throw new UserTypeException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Patch UserType attributes by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Patched each provided attribute of UserType with the given value",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "UserType attribute/value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No UserType found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No UserType attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to patch provided attributes of UserType with the given values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PatchMapping(path = "{id}", consumes = MEDIA_ACCESS_APPLICATION_JSON_PATCH)
    public ResponseEntity<Void> patchExistingOperation(@PathVariable String id, @RequestBody(required = false) List<PatchOperationForm> dtoList)
            throws UserTypeException {
        log.debug("Requesting to patch of userType attributes");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(UserTypeMessageTemplate.MSG_TEMPLATE_USER_TYPE_ID_VALID.getValue(), id);
                if(dtoList != null) {
                    service.applyPatchOnUserType(actualId, dtoList);
                    log.debug("Responding with successful patch of attributes for existing userType");
                    return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
                }
                log.debug("userType patch document is null");
                throw new UserTypeException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED,
                        new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
            } catch (NumberFormatException e) {
                log.debug(UserTypeMessageTemplate.MSG_TEMPLATE_USER_TYPE_ID_INVALID.getValue(), id);
                throw new UserTypeException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(UserTypeMessageTemplate.MSG_TEMPLATE_USER_TYPE_ID_EMPTY.getValue());
        throw new UserTypeException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Get all UserType details")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available UserTypes and their details",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = UserTypeVo.class))) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping
    public Set<UserTypeVo> getAllOperationNaturallyOrdered() {
        log.debug("Requesting all available userTypes by their natural orders");
        Set<UserTypeVo> naturallyOrderedUserTypes = service.retrieveAllByNaturalOrdering();
        log.debug("Responding with all available userTypes by their natural orders");
        return naturallyOrderedUserTypes;
    }

    @Operation(summary = "Get all UserType details by name")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Retrieve all available UserTypes and their details that match the given name",
            content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = UserTypeVo.class))) }),
        @ApiResponse(responseCode = "400", description = "UserType name is invalid",
            content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
        @ApiResponse(responseCode = "404", description = "No UserTypes available with the given name",
            content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("name/{name}")
    public List<UserTypeVo> getAllUserTypesByName(@PathVariable String name) throws UserTypeException {
        log.debug("Requesting all available userTypes with given name");
        if(StringUtils.hasText(StringUtils.trimWhitespace(name))) {
            List<UserTypeVo> matchedByNames = service.retrieveAllMatchingDetailsByName(name);
            log.debug("Responding with all available userTypes with given name");
            return matchedByNames;
        }
        log.debug("userType name is empty");
        throw new UserTypeException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "name", name });
    }

    @Operation(summary = "Get UserType details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve the details of UserType that matches the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = UserTypeVo.class)) }),
            @ApiResponse(responseCode = "400", description = "UserType id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No UserType found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("{id}")
    public UserTypeVo getOperationDetailsById(@PathVariable String id) throws UserTypeException {
        log.debug("Requesting all available userTypes by its id");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(UserTypeMessageTemplate.MSG_TEMPLATE_USER_TYPE_ID_VALID.getValue(), id);
                UserTypeVo userTypeDetails = service.retrieveDetailsById(actualId);
                log.debug("Responding with successful retrieval of existing userType details by id");
                return userTypeDetails;
            } catch (NumberFormatException e) {
                log.debug(UserTypeMessageTemplate.MSG_TEMPLATE_USER_TYPE_ID_INVALID.getValue(), id);
                throw new UserTypeException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(UserTypeMessageTemplate.MSG_TEMPLATE_USER_TYPE_ID_EMPTY.getValue());
        throw new UserTypeException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

}
