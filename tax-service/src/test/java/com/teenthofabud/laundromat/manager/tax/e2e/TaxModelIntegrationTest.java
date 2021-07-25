package com.teenthofabud.laundromat.manager.tax.e2e;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.teenthofabud.core.common.data.entity.TypeModelEntity;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.form.TypeModelForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.data.vo.TypeModelVo;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.laundromat.manager.tax.error.TaxErrorCode;
import com.teenthofabud.laundromat.manager.tax.lov.data.TaxLOVEntity;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelEntity;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelForm;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelVo;
import io.specto.hoverfly.junit.core.Hoverfly;
import io.specto.hoverfly.junit.core.HoverflyConfig;
import io.specto.hoverfly.junit.core.HoverflyMode;
import io.specto.hoverfly.junit.core.SimulationSource;
import io.specto.hoverfly.junit.core.config.LocalHoverflyConfig;
import org.junit.Assert;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
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
import java.net.URISyntaxException;
import java.nio.file.Path;
import java.nio.file.Paths;
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
public class TaxModelIntegrationTest {

    private static final String MEDIA_TAX_APPLICATION_JSON_PATCH = "application/json-patch+json";

    private static final Long CREATED_BY_USER_ID = 1L;

    private static final String SIMULATION_BASE_LOCATION = "simulation/type-service";
    private static final String SIMULATION_FILE_PATH = String.join("/", SIMULATION_BASE_LOCATION, "simulation-v3.json");

    private static final String TAX_MODEL_URI = "/model";
    private static final String TAX_MODEL_URI_BY_ID = "/model/{id}";
    private static final String TAX_MODEL_URI_BY_NAME = "/model/name/{name}";
    private static final String TAX_MODEL_URI_BY_TAX_LOV_ID = "/model/taxlovid/{taxLovId}";
    private static final String TAX_MODEL_URI_BY_CURRENCY_TYPE_MODEL_ID = "/model/currencytypemodelid/{currencyTaxModelId}";

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private ObjectMapper om;

    @Autowired
    private EntityManager em;

    @Value("${lms.tax.type-service.port}")
    private int typeServicePort;

    private Hoverfly hoverfly;

    private TypeModelForm currencyTypeModelForm;
    private TypeModelEntity currencyTypeModelEntity;
    private TaxLOVEntity taxLovEntity1;
    private TaxLOVEntity taxLovEntity2;
    private TypeModelVo currencyTypeModelVo;
    private TypeModelVo taxLovVo1;
    private TypeModelVo taxLovVo2;

    private TaxModelForm taxModelForm1;
    private TaxModelForm taxModelForm2;
    private TaxModelVo taxModelVo1;
    private TaxModelVo taxModelVo2;
    private TaxModelVo taxModelVo3;
    private TaxModelEntity taxModelEntity1;
    private TaxModelEntity taxModelEntity2;
    private TaxModelEntity taxModelEntity3;
    private List<PatchOperationForm> patches;

    @BeforeAll
    private void setUp() throws URISyntaxException {
        currencyTypeModelForm = new TypeModelForm();
        currencyTypeModelForm.setId(2L);
        currencyTypeModelForm.setName("Currency Type Model 1");

        taxModelForm1 = new TaxModelForm();
        taxModelForm1.setName("Demo Tax");
        taxModelForm1.setDescription("This belongs to taxLovId 1 and currencyTypeModelId 2 for e2e testing");
        taxModelForm1.setTaxLovId(1L);
        taxModelForm1.setCurrencyTypeModel(currencyTypeModelForm);
        taxModelForm1.setRate(8F);

        taxModelForm2 = new TaxModelForm();
        taxModelForm2.setName("Another Tax");
        taxModelForm2.setDescription("This is for e2e testing of services");
        taxModelForm2.setTaxLovId(2L);
        taxModelForm2.setCurrencyTypeModel(currencyTypeModelForm);
        taxModelForm2.setRate(8F);

        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", "Sample Tax"),
                new PatchOperationForm("replace", "/active", "true"),
                new PatchOperationForm("replace", "/description", "Patching description attribute of this Tax Model resource"));

        taxLovVo1 = new TypeModelVo();
        taxLovVo1.setId(1L);
        taxLovVo1.setName("Tax LOV 1");

        taxLovVo2 = new TypeModelVo();
        taxLovVo2.setId(2L);
        taxLovVo2.setName("Tax LOV 2");

        taxLovEntity1 = new TaxLOVEntity();
        taxLovEntity1.setActive(Boolean.TRUE);
        taxLovEntity1.setCreatedBy(CREATED_BY_USER_ID);
        taxLovEntity1.setCreatedOn(LocalDateTime.now());
        taxLovEntity1.setModifiedBy(CREATED_BY_USER_ID);
        taxLovEntity1.setModifiedOn(LocalDateTime.now());
        taxLovEntity1.setVersion(0);
        taxLovEntity1.setName("Tax LOV 1");

        taxLovEntity2 = new TaxLOVEntity();
        taxLovEntity2.setActive(Boolean.TRUE);
        taxLovEntity2.setCreatedBy(CREATED_BY_USER_ID);
        taxLovEntity2.setCreatedOn(LocalDateTime.now());
        taxLovEntity2.setModifiedBy(CREATED_BY_USER_ID);
        taxLovEntity2.setModifiedOn(LocalDateTime.now());
        taxLovEntity2.setVersion(0);
        taxLovEntity2.setName("Tax LOV 2");

        currencyTypeModelVo = new TypeModelVo();
        currencyTypeModelVo.setId(2L);
        currencyTypeModelVo.setName("Currency Type Model 1");

        taxModelVo1 = new TaxModelVo();
        taxModelVo1.setId(1L);
        taxModelVo1.setActive(Boolean.TRUE);
        taxModelVo1.setName("Test Tax Model 1");
        taxModelVo1.setDescription("This belongs to taxLovId 1 and currencyTypeModelId 2 for e2e testing");
        taxModelVo1.setRate(5F);
        taxModelVo1.setTaxLov(taxLovVo1);
        taxModelVo1.setCurrencyTypeModel(currencyTypeModelVo);

        taxModelVo2 = new TaxModelVo();
        taxModelVo2.setId(2L);
        taxModelVo2.setActive(Boolean.TRUE);
        taxModelVo2.setName("Test Tax Model 2");
        taxModelVo2.setDescription("This belongs to taxLovId 1 and currencyTypeModelId 2 for e2e testing");
        taxModelVo2.setRate(5F);
        taxModelVo2.setTaxLov(taxLovVo1);
        taxModelVo2.setCurrencyTypeModel(currencyTypeModelVo);

        taxModelVo3 = new TaxModelVo();
        taxModelVo3.setId(3L);
        taxModelVo3.setActive(Boolean.FALSE);
        taxModelVo3.setName("Test Tax Model 3");
        taxModelVo3.setDescription("This belongs to taxLovId 22 and currencyTypeModelId 2 for e2e testing");
        taxModelVo3.setRate(53F);
        taxModelVo3.setTaxLov(taxLovVo2);
        taxModelVo3.setCurrencyTypeModel(currencyTypeModelVo);

        currencyTypeModelEntity = new TypeModelEntity();
        currencyTypeModelEntity.setId(2L);
        currencyTypeModelEntity.setName("Currency Type Model 1");

        taxModelEntity1 = new TaxModelEntity();
        taxModelEntity1.setActive(Boolean.TRUE);
        taxModelEntity1.setCreatedBy(CREATED_BY_USER_ID);
        taxModelEntity1.setCreatedOn(LocalDateTime.now());
        taxModelEntity1.setModifiedBy(CREATED_BY_USER_ID);
        taxModelEntity1.setModifiedOn(LocalDateTime.now());
        taxModelEntity1.setVersion(0);
        taxModelEntity1.setName("Test Tax Model 1");
        taxModelEntity1.setDescription("This belongs to taxLovId 1 and currencyTypeModelId 2 for e2e testing");
        taxModelEntity1.setRate(5F);
        taxModelEntity1.setCurrencyTypeModel(currencyTypeModelEntity);

        taxModelEntity2 = new TaxModelEntity();
        taxModelEntity2.setActive(Boolean.TRUE);
        taxModelEntity2.setCreatedBy(CREATED_BY_USER_ID);
        taxModelEntity2.setCreatedOn(LocalDateTime.now());
        taxModelEntity2.setModifiedBy(CREATED_BY_USER_ID);
        taxModelEntity2.setModifiedOn(LocalDateTime.now());
        taxModelEntity2.setVersion(0);
        taxModelEntity2.setName("Test Tax Model 2");
        taxModelEntity2.setDescription("This belongs to taxLovId 1 and currencyTypeModelId 2 for e2e testing");
        taxModelEntity2.setRate(5F);
        taxModelEntity2.setCurrencyTypeModel(currencyTypeModelEntity);

        taxModelEntity3 = new TaxModelEntity();
        taxModelEntity3.setActive(Boolean.FALSE);
        taxModelEntity3.setCreatedBy(CREATED_BY_USER_ID);
        taxModelEntity3.setCreatedOn(LocalDateTime.now());
        taxModelEntity3.setModifiedBy(CREATED_BY_USER_ID);
        taxModelEntity3.setModifiedOn(LocalDateTime.now());
        taxModelEntity3.setVersion(0);
        taxModelEntity3.setName("Test Tax Model 3");
        taxModelEntity3.setDescription("This belongs to taxLovId 22 and currencyTypeModelId 2 for e2e testing");
        taxModelEntity3.setRate(53F);
        taxModelEntity3.setCurrencyTypeModel(currencyTypeModelEntity);

        om.registerModule(new Jdk8Module());
        om.registerModule(new JavaTimeModule());

        LocalHoverflyConfig localHoverflyConfig = HoverflyConfig.localConfigs();

        ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
        Path simulationBaseLocationPath = Paths.get(classLoader.getResource(SIMULATION_BASE_LOCATION).toURI());

        localHoverflyConfig
                .addCommands("-response-body-files-path", simulationBaseLocationPath.toString())
                .disableTlsVerification()
                .asWebServer()
                .proxyPort(typeServicePort);

        hoverfly = new Hoverfly(localHoverflyConfig, HoverflyMode.SIMULATE);
        hoverfly.start();
        hoverfly.simulate(SimulationSource.classpath(SIMULATION_FILE_PATH));

    }

    @AfterAll
    private void tearDown() {
        if(hoverfly != null) {
            hoverfly.close();
        }
    }

    @BeforeEach
    private void init() {
        taxLovEntity1 = em.merge(taxLovEntity1);
        taxLovEntity2 = em.merge(taxLovEntity2);

        taxModelEntity1.setTaxLov(taxLovEntity1);
        taxModelEntity2.setTaxLov(taxLovEntity1);
        taxModelEntity3.setTaxLov(taxLovEntity2);

        em.merge(taxModelEntity1);
        em.merge(taxModelEntity2);
        em.merge(taxModelEntity3);
    }

    @AfterEach
    private void destroy() {
        taxModelEntity1.setTaxLov(null);
        taxModelEntity2.setTaxLov(null);
        taxModelEntity3.setTaxLov(null);

        em.remove(taxModelEntity1);
        em.remove(taxModelEntity2);
        em.remove(taxModelEntity3);

        em.remove(taxLovEntity1);
        em.remove(taxLovEntity2);

        currencyTypeModelForm = new TypeModelForm();
        currencyTypeModelForm.setId(2L);
        currencyTypeModelForm.setName("Currency Type Model 1");

        taxModelForm1 = new TaxModelForm();
        taxModelForm1.setName("Demo Tax");
        taxModelForm1.setDescription("This belongs to taxLovId 1 and currencyTypeModelId 2 for e2e testing");
        taxModelForm1.setTaxLovId(1L);
        taxModelForm1.setCurrencyTypeModel(currencyTypeModelForm);
        taxModelForm1.setRate(8F);

        taxModelForm2 = new TaxModelForm();
        taxModelForm2.setName("Another Tax");
        taxModelForm2.setDescription("This is for e2e testing of services");
        taxModelForm2.setTaxLovId(2L);
        taxModelForm2.setCurrencyTypeModel(currencyTypeModelForm);
        taxModelForm2.setRate(8F);

        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", "Sample Tax"),
                new PatchOperationForm("replace", "/active", "true"),
                new PatchOperationForm("replace", "/description", "Patching description attribute of this Tax Model resource"));
    }

    @Test
    public void test_TaxModel_Post_ShouldReturn_201Response_And_NewTaxModelId_WhenPosted_WithValidTaxModelForm() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(post(TAX_MODEL_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(taxModelForm1)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assert.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @Test
    public void test_TaxModel_Post_ShouldReturn_400Response_And_ErrorCode_LMS_TAX_001_WhenRequested_WithEmptyName() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";
        taxModelForm1.setName("");

        mvcResult = mockMvc.perform(post(TAX_MODEL_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(taxModelForm1)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_TaxModel_Post_ShouldReturn_409Response_And_ErrorCode_LMS_TAX_004_WhenRequested_WithDuplicateTaxModel() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_EXISTS.getErrorCode();
        String field1Name = "name";
        taxModelForm1.setName("Test Tax Model 1");

        mvcResult = mockMvc.perform(post(TAX_MODEL_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(taxModelForm1)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.CONFLICT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field1Name));
    }

    @Test
    public void test_TaxModel_Post_ShouldReturn_422Response_And_ErrorCode_LMS_TAX_003_WhenPosted_WithNoTaxModelForm() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = mockMvc.perform(post(TAX_MODEL_URI)
                .contentType(MediaType.APPLICATION_JSON))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));

    }

    @ParameterizedTest
    @ValueSource(longs = { -3, 33, 3 })
    public void test_TaxModel_Post_ShouldReturn_400Response_And_ErrorCode_LMS_TAX_001_WhenPostedWith_InvalidAbsentInactive_TaxModelId(Long taxLovId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "taxLovId";
        String message = "invalid";
        taxModelForm1.setTaxLovId(taxLovId);

        mvcResult = mockMvc.perform(post(TAX_MODEL_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(taxModelForm1)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));

    }

    @Test
    public void test_TaxModel_Get_ShouldReturn_200Response_And_TaxModelListNaturallyOrdered_WhenRequested_ForAllTaxModels() throws Exception {
        MvcResult mvcResult = null;
        Set<TaxModelVo> studentList = new TreeSet<>(Arrays.asList(taxModelVo1, taxModelVo2));
        long expectedTaxModelVoCount = 3;

        mvcResult = this.mockMvc.perform(get(TAX_MODEL_URI))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(expectedTaxModelVoCount, om.readValue(mvcResult.getResponse().getContentAsString(), TaxModelVo[].class).length);
    }

    @Test
    public void test_TaxModel_Get_ShouldReturn_200Response_And_TaxModelDetails_WhenRequested_ById() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(TAX_MODEL_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(om.writeValueAsString(taxModelVo1), mvcResult.getResponse().getContentAsString());
        Assert.assertEquals(taxModelVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), TaxModelVo.class).getId());
    }

    @Test
    public void test_TaxModel_Get_ShouldReturn_400Response_And_ErrorCode_LMS_TAX_001_WhenRequested_ByEmptyId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(TAX_MODEL_URI_BY_ID, " "))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_TaxModel_Get_ShouldReturn_400Response_And_ErrorCode_LMS_TAX_001_WhenRequested_ByInvalidId() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(TAX_MODEL_URI_BY_ID, "r"))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_TaxModel_Get_ShouldReturn_400Response_And_ErrorCode_LMS_TAX_002_WhenRequested_ByAbsentId() throws Exception {
        Long id = 111l;
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(TAX_MODEL_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_TaxModel_Get_ShouldReturn_200Response_And_MatchingTaxModelDetails_WhenRequested_ByName() throws Exception {
        String name = "Test";
        List<TaxModelVo> students = Arrays.asList(taxModelVo1, taxModelVo2);
        MvcResult mvcResult = null;
        long expectedTaxModelVoCount = 3;

        mvcResult = this.mockMvc.perform(get(TAX_MODEL_URI_BY_NAME, name))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(expectedTaxModelVoCount, om.readValue(mvcResult.getResponse().getContentAsString(), TaxModelVo[].class).length);
    }

    @Test
    public void test_TaxModel_Get_ShouldReturn_400Response_And_ErrorCode_LMS_TAX_001_WhenRequested_ByEmptyName() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";

        mvcResult = this.mockMvc.perform(get(TAX_MODEL_URI_BY_NAME, " "))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_TaxModel_Get_ShouldReturn_404Response_And_ErrorCode_LMS_TAX_002_WhenRequested_ByAbsentName() throws Exception {
        MvcResult mvcResult = null;
        String name = "kk";
        String errorCode = TaxErrorCode.TAX_NOT_FOUND.getErrorCode();
        String fieldName = "name";

        mvcResult = this.mockMvc.perform(get(TAX_MODEL_URI_BY_NAME, name))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(name));
    }

    @Test
    public void test_TaxModel_Delete_ShouldReturn_204Response_And_NoResponseBody_WhenDeleted_ById() throws Exception {
        Long id = 2l;
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(delete(TAX_MODEL_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_TaxModel_Delete_ShouldReturn_400Response_And_ErrorCode_LMS_TAX_001__WhenDeleted_ByEmptyId() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(TAX_MODEL_URI_BY_ID, " "))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_TaxModel_Delete_ShouldReturn_422Response_And_ErrorCode_LMS_TAX_003_WhenDeleted_ByInvalidId() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(TAX_MODEL_URI_BY_ID, "r"))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_TaxModel_Delete_ShouldReturn_400Response_And_ErrorCode_LMS_TAX_005_WhenDeleted_ByInactiveId() throws Exception {
        Long id = 3l;
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(delete(TAX_MODEL_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_TaxModel_Delete_ShouldReturn_404Response_And_ErrorCode_LMS_TAX_002_WhenDeleted_ByAbsentId() throws Exception {
        Long id = 111l;
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(TAX_MODEL_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_TaxModel_Put_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndTaxModelDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        taxModelForm1.setName("Ferran");

        mvcResult = this.mockMvc.perform(put(TAX_MODEL_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(taxModelForm1)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_TaxModel_Put_ShouldReturn_400Response_And_ErrorCode_LMS_TAX_001_WhenUpdated_ByEmptyId_AndTaxModelDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(TAX_MODEL_URI_BY_ID, " ")
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(taxModelForm1)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_TaxModel_Put_ShouldReturn_422Response_And_ErrorCode_LMS_TAX_003_WhenUpdated_ByInvalidId_AndTaxModelDetails() throws Exception {
        String id = "r";
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(TAX_MODEL_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(taxModelForm1)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id));
    }

    @Test
    public void test_TaxModel_Put_ShouldReturn_404Response_And_ErrorCode_LMS_TAX_002_WhenUpdated_ByAbsentId_AndTaxModelDetails() throws Exception {
        Long id = 41l;
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(TAX_MODEL_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(taxModelForm1)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_TaxModel_Put_ShouldReturn_400Response_And_ErrorCode_LMS_TAX_005_WhenUpdated_ByInactiveId_AndTaxModelDetails() throws Exception {
        Long id = 3l;
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(put(TAX_MODEL_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(taxModelForm1)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @ParameterizedTest
    @ValueSource(longs = { -3 })
    public void test_TaxModel_Put_ShouldReturn_400Response_And_ErrorCode_LMS_TAX_001_WhenUpdatedWith_InvalidAbsentInactive_TaxModelId(Long taxLovId) throws Exception {
        Long id = 1L;
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "taxLovId";
        String message = "invalid";
        taxModelForm1.setTaxLovId(taxLovId);

        mvcResult = mockMvc.perform(put(TAX_MODEL_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(taxModelForm1)))
                                                                                                                           .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));

    }

    @ParameterizedTest
    @ValueSource(longs = { 33, 3 })
    public void test_TaxModel_Put_ShouldReturn_400Response_And_ErrorCode_LMS_TAX_001_WhenUpdatedWith_AbsentInactive_TaxModelId(Long taxLovId) throws Exception {
        Long id = 1L;
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "taxLovId";
        String message = "invalid";
        taxModelForm1.setTaxLovId(taxLovId);

        mvcResult = mockMvc.perform(put(TAX_MODEL_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(taxModelForm1)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));

    }

    @Test
    public void test_TaxModel_Put_ShouldReturn_422Response_And_ErrorCode_LMS_TAX_003_WhenUpdated_ById_AndNoTaxModelDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(put(TAX_MODEL_URI_BY_ID, id)
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
    public void test_TaxModel_Put_ShouldReturn_400Response_And_ErrorCode_LMS_TAX_001_WhenRequested_ById_AndInvalidName() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";
        taxModelForm1.setName("");

        mvcResult = mockMvc.perform(put(TAX_MODEL_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(taxModelForm1)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_TaxModel_Put_ShouldReturn_422Response_And_ErrorCode_LMS_TAX_003_WhenUpdated_ById_AndEmptyTaxModelDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "fields are expected with new values";

        mvcResult = this.mockMvc.perform(put(TAX_MODEL_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(new TaxModelForm())))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_TaxModel_Put_ShouldReturn_409Response_And_ErrorCode_LMS_TAX_004_WhenUpdated_ById_AndDuplicateTaxModelDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_EXISTS.getErrorCode();
        String field1Name = "name";
        taxModelForm1.setName(taxModelEntity1.getName());

        mvcResult = mockMvc.perform(put(TAX_MODEL_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(taxModelForm1)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.CONFLICT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field1Name));
    }

    @Test
    public void test_TaxModel_Patch_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndTaxModelDetails() throws Exception {
        Long id = 2l;
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(patch(TAX_MODEL_URI_BY_ID, id)
                .contentType(MEDIA_TAX_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_TaxModel_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenUpdated_ByEmptyId_AndTaxModelDetails() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(TAX_MODEL_URI_BY_ID, " ")
                .contentType(MEDIA_TAX_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @ParameterizedTest
    @ValueSource(strings = { "-3", "33", "3" })
    public void test_TaxModel_Patch_ShouldReturn_400Response_And_ErrorCode_LMS_TAX_001_WhenUpdatedWith_InvalidAbsentInactive_TaxModelId(String fieldValue) throws Exception {
        Long id = 1L;
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "taxLovId";
        String message = "invalid";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, fieldValue));

        mvcResult = mockMvc.perform(patch(TAX_MODEL_URI_BY_ID, id)
                .contentType(MEDIA_TAX_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));

    }

    @Test
    public void test_TaxModel_Patch_ShouldReturn_422Response_And_ErrorCode_LMS_TAX_003_WhenUpdated_ByInvalidId_AndTaxModelDetails() throws Exception {
        String id = "r";
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(TAX_MODEL_URI_BY_ID, id)
                .contentType(MEDIA_TAX_APPLICATION_JSON_PATCH)
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
    public void test_TaxModel_Patch_ShouldReturn_404Response_And_ErrorCode_LMS_TAX_002_WhenUpdated_ByAbsentId_AndTaxModelDetails() throws Exception {
        Long id = 411l;
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(TAX_MODEL_URI_BY_ID, id)
                .contentType(MEDIA_TAX_APPLICATION_JSON_PATCH)
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
    public void test_TaxModel_Patch_ShouldReturn_409Response_And_ErrorCode_LMS_TAX_002_WhenUpdated_ById_AndDuplicateTaxModelDetails() throws Exception {
        Long id = 2l;
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_EXISTS.getErrorCode();
        String fieldName = "name";
        String fieldValue = taxModelEntity2.getName();
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/" + fieldName, fieldValue));


        mvcResult = this.mockMvc.perform(patch(TAX_MODEL_URI_BY_ID, id)
                .contentType(MEDIA_TAX_APPLICATION_JSON_PATCH)
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
    public void test_TaxModel_Patch_ShouldReturn_422Response_And_ErrorCode_LMS_TAX_003_WhenUpdated_ById_AndNoTaxModelDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "patch";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(patch(TAX_MODEL_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_TaxModel_Patch_ShouldReturn_400Response_And_ErrorCode_LMS_TAX_001_WhenRequested_ById_AndInvalidActive() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "active";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/active", "x"));

        mvcResult = mockMvc.perform(patch(TAX_MODEL_URI_BY_ID, id)
                .contentType(MEDIA_TAX_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_TaxModel_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidName() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", ""));

        mvcResult = mockMvc.perform(patch(TAX_MODEL_URI_BY_ID, id)
                .contentType(MEDIA_TAX_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_TaxModel_Patch_ShouldReturn_400Response_And_ErrorCode_LMS_TAX_001_WhenRequested_ById_AndInvalidDefinitionOfTaxModelAttribute() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "path";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/x", "x"));

        mvcResult = mockMvc.perform(patch(TAX_MODEL_URI_BY_ID, id)
                .contentType(MEDIA_TAX_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_TaxModel_Get_ShouldReturn_200Response_And_TaxModelDetails_WhenRequested_ByTaxLovId() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        long expectedTaxModelCount = 2;

        mvcResult = this.mockMvc.perform(get(TAX_MODEL_URI_BY_TAX_LOV_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(om.writeValueAsString(Arrays.asList(taxModelVo1, taxModelVo2)), mvcResult.getResponse().getContentAsString());
        Assert.assertEquals(expectedTaxModelCount, om.readValue(mvcResult.getResponse().getContentAsString(), TaxModelVo[].class).length);
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_TaxModel_Get_ShouldReturn_400Response_And_ErrorCode_LMS_TAX_001_WhenRequestedBy_EmptyInvalid_TaxLovId(String taxLovId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "taxLovId";
        String message = "invalid";

        mvcResult = this.mockMvc.perform(get(TAX_MODEL_URI_BY_TAX_LOV_ID, taxLovId))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @ParameterizedTest
    @ValueSource(strings = { "-3", "33", "3" })
    public void test_TaxModel_Get_ShouldReturn_404Response_And_ErrorCode_LMS_TAX_002_WhenRequestedBy_InvalidAbsentInactive_TaxLovId(String taxLovId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_NOT_FOUND.getErrorCode();
        String fieldName = "taxLovId";
        String message = "unavailable";

        mvcResult = this.mockMvc.perform(get(TAX_MODEL_URI_BY_TAX_LOV_ID, taxLovId))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_TaxModel_Get_ShouldReturn_200Response_And_TaxModelDetails_WhenRequested_ByCurrencyTypeModelId() throws Exception {
        Long id = 2l;
        MvcResult mvcResult = null;
        long expectedTaxModelCount = 3;

        mvcResult = this.mockMvc.perform(get(TAX_MODEL_URI_BY_CURRENCY_TYPE_MODEL_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(om.writeValueAsString(Arrays.asList(taxModelVo1, taxModelVo2, taxModelVo3)), mvcResult.getResponse().getContentAsString());
        Assert.assertEquals(expectedTaxModelCount, om.readValue(mvcResult.getResponse().getContentAsString(), TaxModelVo[].class).length);
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r", "-3", "33", "3" })
    public void test_TaxModel_Get_ShouldReturn_400Response_And_ErrorCode_LMS_TAX_001_WhenRequestedBy_EmptyInvalidAbsentInactive_CurrencyTypeModelId(String currencyTypeModelId) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = TaxErrorCode.TAX_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "currencyTypeModelId";

        mvcResult = this.mockMvc.perform(get(TAX_MODEL_URI_BY_CURRENCY_TYPE_MODEL_ID, currencyTypeModelId))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

}