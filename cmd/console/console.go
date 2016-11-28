package main

import (
	"flag"
	"net/http"
	"os"
	"time"

	"github.com/go-kit/kit/log"
	"github.com/gorilla/mux"
	"github.com/jmoiron/sqlx"
	"github.com/prometheus/client_golang/prometheus"
	"golang.org/x/net/context"

	"github.com/tapglue/snaas/core"
	handler "github.com/tapglue/snaas/handler/http"
	"github.com/tapglue/snaas/platform/metrics"
	"github.com/tapglue/snaas/service/app"
)

const (
	component = "console"

	namespaceService = "service"

	storeService = "postgres"

	version = "0.4"
)

// Timeouts
const (
	defaultReadTimeout  = 2 * time.Second
	defaultWriteTimeout = 3 * time.Second
)

// Buildtime variables.
var (
	revision = "0000000-dev"
)

func main() {
	var (
		begin = time.Now()

		listenAddr    = flag.String("listen.adrr", ":8084", "HTTP bind address for main API")
		postgresURL   = flag.String("postgres.url", "", "Postgres URL to connect to")
		telemetryAddr = flag.String("telemetry.addr", ":9002", "HTTP bind address where telemetry is exposed")
	)
	flag.Parse()

	// Setup logging.
	logger := log.NewContext(
		log.NewJSONLogger(os.Stdout),
	).With(
		"caller", log.Caller(3),
		"component", component,
		"revision", revision,
	)

	hostname, err := os.Hostname()
	if err != nil {
		logger.Log("err", err, "lifecycle", "abort")
		os.Exit(1)
	}

	logger = log.NewContext(logger).With("host", hostname)

	// Setup instrumentation.
	go func(addr string) {
		logger.Log(
			"duration", time.Now().Sub(begin).Nanoseconds(),
			"lifecycle", "start",
			"listen", addr,
			"sub", "telemetry",
		)

		http.Handle("/metrics", prometheus.Handler())

		err := http.ListenAndServe(addr, nil)
		if err != nil {
			logger.Log("err", err, "lifecycle", "abort", "sub", "telemetry")
			os.Exit(1)
		}
	}(*telemetryAddr)

	serviceErrCount, serviceOpCount, serviceOpLatency := metrics.KeyMetrics(
		namespaceService,
		metrics.FieldComponent,
		metrics.FieldMethod,
		metrics.FieldNamespace,
		metrics.FieldService,
		metrics.FieldStore,
	)

	// Setup clients.
	pgClient, err := sqlx.Connect(storeService, *postgresURL)
	if err != nil {
		logger.Log("err", err, "lifecycle", "abort")
		os.Exit(1)
	}

	// Setup services.
	var apps app.Service
	apps = app.PostgresService(pgClient)
	apps = app.InstrumentServiceMiddleware(
		component,
		storeService,
		serviceErrCount,
		serviceOpCount,
		serviceOpLatency,
	)(apps)
	apps = app.LogServiceMiddleware(logger, storeService)(apps)

	// Setup middlewares.
	var (
		withConstraints = handler.Chain(
			handler.CtxPrepare(version),
			handler.Log(logger),
			handler.Instrument(component),
			handler.SecureHeaders(),
			handler.DebugHeaders(revision, hostname),
			handler.CORS(),
			handler.HasUserAgent(),
		)
	)

	// Setup Router.
	router := mux.NewRouter()

	router.Methods("GET").Path("/api/apps").Name("appsGet").HandlerFunc(
		handler.Wrap(
			withConstraints,
			handler.AppList(core.AppList(apps)),
		),
	)

	router.Methods("POST").Path("/api/apps").Name("appCreate").HandlerFunc(
		handler.Wrap(
			withConstraints,
			handler.AppCreate(core.AppCreate(apps)),
		),
	)

	router.Methods("GET").PathPrefix("/").Name("root").HandlerFunc(
		handler.Wrap(
			withConstraints,
			func(ctx context.Context, w http.ResponseWriter, r *http.Request) {
				http.StripPrefix("/", http.FileServer(http.Dir("."))).ServeHTTP(w, r)
			},
		),
	)

	// Setup server.
	server := &http.Server{
		Addr:         *listenAddr,
		Handler:      router,
		ReadTimeout:  defaultReadTimeout,
		WriteTimeout: defaultWriteTimeout,
	}

	logger.Log(
		"duration", time.Now().Sub(begin).Nanoseconds(),
		"lifecycle", "start",
		"listen", *listenAddr,
		"sub", "api",
	)

	err = server.ListenAndServe()
	if err != nil {
		logger.Log("err", err, "lifecycle", "abort", "sub", "api")
		os.Exit(1)
	}
}
