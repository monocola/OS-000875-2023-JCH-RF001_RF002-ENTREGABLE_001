package pe.gob.servir.mensajeria.config;

import java.util.HashMap;
import java.util.Map;

import javax.persistence.EntityManagerFactory;
import javax.sql.DataSource;

import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.autoconfigure.jdbc.DataSourceProperties;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.orm.jpa.EntityManagerFactoryBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.mail.SimpleMailMessage;
import org.springframework.orm.jpa.JpaTransactionManager;
import org.springframework.orm.jpa.LocalContainerEntityManagerFactoryBean;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.annotation.EnableTransactionManagement;

import com.zaxxer.hikari.HikariDataSource;

@Configuration
@EnableTransactionManagement
@EnableJpaRepositories(
		entityManagerFactoryRef = "mensajeriaEntityManagerFactory", 
		transactionManagerRef = "mensajeriaTransactionManager", 
		basePackages = { "pe.gob.servir.mensajeria.repository" })
public class RepositoryConfiguration {

	@Primary
	@Bean(name = "mensajeriaDataSourceProperties")
	@ConfigurationProperties("mensajeria.datasource")
	public DataSourceProperties dataSourceProperties() {
		return new DataSourceProperties();
	}

	@Primary
	@Bean(name = "mensajeriaDataSource")
	@ConfigurationProperties("mensajeria.datasource.configuration")
	public DataSource dataSource(
			@Qualifier("mensajeriaDataSourceProperties") DataSourceProperties mensajeriaDataSourceProperties) {
		return mensajeriaDataSourceProperties.initializeDataSourceBuilder().type(HikariDataSource.class).build();
	}

	@Primary
	@Bean(name = "mensajeriaEntityManagerFactory")
	public LocalContainerEntityManagerFactoryBean entityManagerFactory(EntityManagerFactoryBuilder builder,
			@Qualifier("mensajeriaDataSource") DataSource mensajeriaDataSource) {

		Map<String, Object> properties = new HashMap<>();
		properties.put("hibernate.dialect", "org.hibernate.dialect.PostgreSQLDialect");
//		properties.put("hibernate.physical_naming_strategy", "pe.gob.servir.mensajeria.common.PostgreSQLNamingStrategy");
		properties.put("hibernate.session_factory.session_scoped_interceptor",
				"pe.gob.servir.mensajeria.audit.AuditEntityInterceptor");
		properties.put("hibernate.proc.param_null_passing", "true");
		properties.put("hibernate.show_sql", "true");
//        "org.springframework.boot.orm.jpa.hibernate.SpringPhysicalNamingStrategy");
//        properties.put("hibernate.implicit_naming_strategy", 
//        "org.springframework.boot.orm.jpa.hibernate.SpringImplicitNamingStrategy");
//        properties.put("hibernate.hbm2ddl.auto", "update");

		return builder.dataSource(mensajeriaDataSource).packages("pe.gob.servir.mensajeria.model")
				.persistenceUnit("mensajeria").properties(properties).build();
	}

	@Primary
	@Bean(name = "mensajeriaTransactionManager")
	public PlatformTransactionManager transactionManager(
			@Qualifier("mensajeriaEntityManagerFactory") EntityManagerFactory mensajeriaEntityManagerFactory) {
		return new JpaTransactionManager(mensajeriaEntityManagerFactory);
	}
	
}
