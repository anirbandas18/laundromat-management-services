package com.teenthofabud.laundromat.manager.tax.e2e;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.laundromat.manager.tax.error.TaxErrorCode;
import com.teenthofabud.laundromat.manager.tax.lov.data.TaxLOVEntity;
import com.teenthofabud.laundromat.manager.tax.lov.data.TaxLOVForm;
import com.teenthofabud.laundromat.manager.tax.lov.data.TaxLOVVo;
import org.junit.Assert;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.AutoConfigureTestDatabase;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.StringUtils;

import javax.persistence.EntityManager;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.patch;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;

@AutoConfigureMockMvc
@ActiveProfiles("test")
@Transactional
@DirtiesContext(classMode = DirtiesContext.ClassMode.BEFORE_EACH_TEST_METHOD)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT)
@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.ANY)
public class TaxLOVIntegrationTest {

    private static final String MEDIA_TYPE_APPLICATION_JSON_PATCH = "application/json-patch+json";

    private static final Long CREATED_BY_USER_ID = 1L;

    private static final String TAX_LOV_URI = "/lov";
    private static final String TAX_LOV_URI_BY_ID = "/lov/{id}";
    private static final String TAX_LOV_URI_BY_NAME = "/lov/name/{name}";

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private ObjectMapper om;

    @Autowired
    private EntityManager em;

    private TaxLOVForm taxLovForm;
    private TaxLOVVo taxLovVo1;
    private TaxLOVVo taxLovVo2;
    private TaxLOVVo taxLovVo3;
    private TaxLOVVo taxLovVo4;
    private TaxLOVEntity taxLovEntity1;
    private TaxLOVEntity taxLovEntity2;
    private TaxLOVEntity taxLovEntity3;
    private TaxLOVEntity taxLovEntity4;
    private List<PatchOperationForm> patches;

    @BeforeAll
    private void setUp() {
        taxLovForm = new TaxLOVForm();
        taxLovForm.setName("Demo LOV");
        taxLovForm.setDescription("This is for e2e testing of services");

        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", "Sample LOV"),
                new PatchOperationForm("replace", "/active", "true"),
                new PatchOperationForm("replace", "/description", "Patching description attribute of this Tax LOV resource"));

        taxLovVo1 = new TaxLOVVo();
        taxLovVo1.setId(1L);
        taxLovVo1.setActive(Boolean.TRUE);
        taxLovVo1.setName("Test Tax LOV 1");
        taxLovVo1.setDescription("This belongs to group 1 for e2e testing");

        taxLovVo2 = new TaxLOVVo();
        taxLovVo2.setId(2L);
        taxLovVo2.setActive(Boolean.TRUE);
        taxLovVo2.setName("Test Tax LOV 2");
        taxLovVo2.setDescription("This belongs to group 1 for e2e testing");

        taxLovVo3 = new TaxLOVVo();
        taxLovVo3.setId(3L);
        taxLovVo3.setActive(Boolean.TRUE);
        taxLovVo3.setName("Sample Tax LOV 3");
        taxLovVo3.setDescription("This belongs to group 2 for e2e testing");

        taxLovVo4 = new TaxLOVVo();
        taxLovVo4.setId(4L);
        taxLovVo4.setActive(Boolean.FALSE);
        taxLovVo4.setName("Sample Tax LOV 4");
        taxLovVo4.setDescription("This belongs to group 2 for e2e testing");

        taxLovEntity1 = new TaxLOVEntity();
        taxLovEntity1.setActive(Boolean.TRUE);
        taxLovEntity1.setCreatedBy(CREATED_BY_USER_ID);
        taxLovEntity1.setCreatedOn(LocalDateTime.now());
        taxLovEntity1.setModifiedBy(CREATED_BY_USER_ID);
        taxLovEntity1.setModifiedOn(LocalDateTime.now());
        taxLovEntity1.setVersion(0);
        taxLovEntity1.setName("Test Tax LOV 1");
        taxLovEntity1.setDescription("This belongs to group 1 for e2e testing");

        taxLovEntity2 = new TaxLOVEntity();
        taxLovEntity2.setActive(Boolean.TRUE);
        taxLovEntity2.setCreatedBy(CREATED_BY_USER_ID);
        taxLovEntity2.setCreatedOn(LocalDateTime.now());
        taxLovEntity2.setModifiedBy(CREATED_BY_USER_ID);
        taxLovEntity2.setModifiedOn(LocalDateTime.now());
        taxLovEntity2.setVersion(0);
        taxLovEntity2.setName("Test Tax LOV 2");
        taxLovEntity2.setDescription("This belongs to group 1 for e2e testing");


        taxLovEntity3 = new TaxLOVEntity();
        taxLovEntity3.setActive(Boolean.TRUE);
        taxLovEntity3.setCreatedBy(CREATED_BY_USER_ID);
        taxLovEntity3.setCreatedOn(LocalDateTime.now());
        taxLovEntity3.setModifiedBy(CREATED_BY_USER_ID);
        taxLovEntity3.setModifiedOn(LocalDateTime.now());
        taxLovEntity3.setVersion(0);
        taxLovEntity3.setName("Sample Tax LOV 3");
        taxLovEntity3.setDescription("This belongs to group 2 for e2e testing");


        taxLovEntity4 = new TaxLOVEntity();
        taxLovEntity4.setActive(Boolean.FALSE);
        taxLovEntity4.setCreatedBy(CREATED_BY_USER_ID);
        taxLovEntity4.setCreatedOn(LocalDateTime.now());
        taxLovEntity4.setModifiedBy(CREATED_BY_USER_ID);
        taxLovEntity4.setModifiedOn(LocalDateTime.now());
        taxLovEntity4.setVersion(0);
        taxLovEntity4.setName("Sample Tax LOV 4");
        taxLovEntity4.setDescription("This belongs to group 2 for e2e testing");

        om.registerModule(new Jdk8Module());
        om.registerModule(new JavaTimeModule());

    }

    @BeforeEach
    private void init() {
        em.merge(taxLovEntity1);
        em.merge(taxLovEntity2);
        em.merge(taxLovEntity3);
        em.merge(taxLovEntity4);
    }

    @AfterEach
    private void destroy() {
        em.remove(taxLovEntity1);
        em.remove(taxLovEntity2);
        em.remove(taxLovEntity3);
        em.remove(taxLovEntity4);

        taxLovForm = new TaxLOVForm();
        taxLovForm.setName("Demo LOV");
        taxLovForm.setDescription("This is for e2e testing of services");

        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", "Sample LOV"),
                new PatchOperationForm("replace", "/active", "true"),
                new PatchOperationForm("replace", "/description", "Patching description attribute of this Tax LOV resource"));
    }

    @Test
    public void test_TaxLOV_Post_ShouldReturn_201Response_And_NewTaxLOVId_WhenPosted_WithValidTaxLOVForm() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(post(TAX_LOV_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(taxLovForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assert.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @Test
    public void test_TaxLOV_Post_ShouldReturn_400Response_And_ErrorCode_LMS_TAX_001_WhenRequested_WithEmptyName() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";
        taxLovForm.setName("");

        mvcResult = mockMvc.perform(post(TAX_LOV_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(taxLovForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_TaxLOV_Post_ShouldReturn_409Response_And_ErrorCode_LMS_TAX_004_WhenRequested_WithDuplicateTaxLOV() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_EXISTS.getErrorCode();
        String field1Name = "name";
        taxLovForm.setName("Test Tax LOV 1");

        mvcResult = mockMvc.perform(post(TAX_LOV_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(taxLovForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.CONFLICT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field1Name));
    }

    @Test
    public void test_TaxLOV_Post_ShouldReturn_422Response_And_ErrorCode_LMS_TAX_003_WhenPosted_WithNoTaxLOVForm() throws Exception {
        long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = mockMvc.perform(post(TAX_LOV_URI)
                .contentType(MediaType.APPLICATION_JSON))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));

    }

    @Test
    public void test_TaxLOV_Get_ShouldReturn_200Response_And_TaxLOVListNaturallyOrdered_WhenRequested_ForAllTaxLOVs() throws Exception {
        MvcResult mvcResult = null;
        Set<TaxLOVVo> studentList = new TreeSet<>(Arrays.asList(taxLovVo1, taxLovVo2, taxLovVo3, taxLovVo4));
        long expectedTaxLOVVoCount = 4;

        mvcResult = this.mockMvc.perform(get(TAX_LOV_URI))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(expectedTaxLOVVoCount, om.readValue(mvcResult.getResponse().getContentAsString(), TaxLOVVo[].class).length);
    }

    @Test
    public void test_TaxLOV_Get_ShouldReturn_200Response_And_TaxLOVDetails_WhenRequested_ById() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(TAX_LOV_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(om.writeValueAsString(taxLovVo1), mvcResult.getResponse().getContentAsString());
        Assert.assertEquals(taxLovVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), TaxLOVVo.class).getId());
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_TaxLOV_Get_ShouldReturn_400Response_And_ErrorCode_LMS_TAX_001_WhenRequestedBy_EmptyInvalidId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(TAX_LOV_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_TaxLOV_Get_ShouldReturn_400Response_And_ErrorCode_LMS_TAX_002_WhenRequested_ByAbsentId() throws Exception {
        Long id = 111l;
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(TAX_LOV_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_TaxLOV_Get_ShouldReturn_200Response_And_MatchingTaxLOVDetails_WhenRequested_ByName() throws Exception {
        String name = "Test";
        List<TaxLOVVo> students = Arrays.asList(taxLovVo1, taxLovVo2);
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(TAX_LOV_URI_BY_NAME, name))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(2, om.readValue(mvcResult.getResponse().getContentAsString(), TaxLOVVo[].class).length);
    }

    @Test
    public void test_TaxLOV_Get_ShouldReturn_400Response_And_ErrorCode_LMS_TAX_001_WhenRequested_ByEmptyName() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";

        mvcResult = this.mockMvc.perform(get(TAX_LOV_URI_BY_NAME, " "))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_TaxLOV_Get_ShouldReturn_404Response_And_ErrorCode_LMS_TAX_002_WhenRequested_ByAbsentName() throws Exception {
        MvcResult mvcResult = null;
        String name = "kk";
        String errorCode = TaxErrorCode.TAX_NOT_FOUND.getErrorCode();
        String fieldName = "name";

        mvcResult = this.mockMvc.perform(get(TAX_LOV_URI_BY_NAME, name))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(name));
    }

    @Test
    public void test_TaxLOV_Delete_ShouldReturn_204Response_And_NoResponseBody_WhenDeleted_ById() throws Exception {
        Long id = 3l;
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(delete(TAX_LOV_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_TaxLOV_Delete_ShouldReturn_400Response_And_ErrorCode_LMS_TAX_001__WhenDeleted_ByEmptyId() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(TAX_LOV_URI_BY_ID, " "))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_TaxLOV_Delete_ShouldReturn_422Response_And_ErrorCode_LMS_TAX_003_WhenDeleted_ByInvalidId() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(TAX_LOV_URI_BY_ID, "r"))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_TaxLOV_Delete_ShouldReturn_400Response_And_ErrorCode_LMS_TAX_005_WhenDeleted_ByInactiveId() throws Exception {
        Long id = 4l;
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(delete(TAX_LOV_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_TaxLOV_Delete_ShouldReturn_404Response_And_ErrorCode_LMS_TAX_002_WhenDeleted_ByAbsentId() throws Exception {
        Long id = 111l;
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(TAX_LOV_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_TaxLOV_Put_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndTaxLOVDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        taxLovForm.setName("Ferran");

        mvcResult = this.mockMvc.perform(put(TAX_LOV_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(taxLovForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_TaxLOV_Put_ShouldReturn_400Response_And_ErrorCode_LMS_TAX_001_WhenUpdatedBy_EmptyInvalidId_AndTaxLOVDetails(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(TAX_LOV_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(taxLovForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_TaxLOV_Put_ShouldReturn_404Response_And_ErrorCode_LMS_TAX_002_WhenUpdated_ByAbsentId_AndTaxLOVDetails() throws Exception {
        Long id = 41l;
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(TAX_LOV_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(taxLovForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_TaxLOV_Put_ShouldReturn_400Response_And_ErrorCode_LMS_TAX_005_WhenUpdated_ByInactiveId_AndTaxLOVDetails() throws Exception {
        Long id = 4l;
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(put(TAX_LOV_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(taxLovForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_TaxLOV_Put_ShouldReturn_422Response_And_ErrorCode_LMS_TAX_003_WhenUpdated_ById_AndNoTaxLOVDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(put(TAX_LOV_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_TaxLOV_Put_ShouldReturn_400Response_And_ErrorCode_LMS_TAX_001_WhenRequested_ById_AndInvalidName() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";
        taxLovForm.setName("");

        mvcResult = mockMvc.perform(put(TAX_LOV_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(taxLovForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_TaxLOV_Put_ShouldReturn_422Response_And_ErrorCode_LMS_TAX_003_WhenUpdated_ById_AndEmptyTaxLOVDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "fields are expected with new values";

        mvcResult = this.mockMvc.perform(put(TAX_LOV_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(new TaxLOVForm())))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_TaxLOV_Put_ShouldReturn_409Response_And_ErrorCode_LMS_TAX_004_WhenUpdated_ById_AndDuplicateTaxLOVDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_EXISTS.getErrorCode();
        String field1Name = "name";
        taxLovForm.setName(taxLovEntity1.getName());

        mvcResult = mockMvc.perform(put(TAX_LOV_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(taxLovForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.CONFLICT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field1Name));
    }

    @Test
    public void test_TaxLOV_Patch_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndTaxLOVDetails() throws Exception {
        Long id = 4l;
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(patch(TAX_LOV_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_TaxLOV_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenUpdated_ByEmptyId_AndTaxLOVDetails() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(TAX_LOV_URI_BY_ID, " ")
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_TaxLOV_Patch_ShouldReturn_400Response_And_ErrorCode_LMS_TAX_003_WhenUpdated_ByInvalidId_AndTaxLOVDetails() throws Exception {
        String id = "r";
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(TAX_LOV_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id));
    }

    @Test
    public void test_TaxLOV_Patch_ShouldReturn_404Response_And_ErrorCode_LMS_TAX_002_WhenUpdated_ByAbsentId_AndTaxLOVDetails() throws Exception {
        Long id = 411l;
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(TAX_LOV_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_TaxLOV_Patch_ShouldReturn_409Response_And_ErrorCode_LMS_TAX_002_WhenUpdated_ById_AndDuplicateTaxLOVDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_EXISTS.getErrorCode();
        String fieldName = "name";
        String fieldValue = "Sample Tax LOV 3";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", fieldValue));


        mvcResult = this.mockMvc.perform(patch(TAX_LOV_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.CONFLICT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldValue));
    }

    @Test
    public void test_TaxLOV_Patch_ShouldReturn_422Response_And_ErrorCode_LMS_TAX_003_WhenUpdated_ById_AndNoTaxLOVDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "patch";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(patch(TAX_LOV_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_TaxLOV_Patch_ShouldReturn_400Response_And_ErrorCode_LMS_TAX_001_WhenRequested_ById_AndInvalidActive() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "active";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/active", "x"));

        mvcResult = mockMvc.perform(patch(TAX_LOV_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_TaxLOV_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidName() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", " "));

        mvcResult = mockMvc.perform(patch(TAX_LOV_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_TaxLOV_Patch_ShouldReturn_400Response_And_ErrorCode_LMS_TAX_001_WhenRequested_ById_AndInvalidDefinitionOfTaxLOVAttribute() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "path";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/x", "x"));

        mvcResult = mockMvc.perform(patch(TAX_LOV_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

}