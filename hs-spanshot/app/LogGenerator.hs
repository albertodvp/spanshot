module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import System.IO (hFlush, stdout)
import System.Random (randomRIO)

-- | Error types that can be generated
data ErrorType = PythonError | JavaError | NodeError
    deriving (Enum, Bounded, Show)

-- | Log levels
data LogLevel = DEBUG | INFO | WARN | ERROR
    deriving (Show)

-- | Configuration for the log generator
data Config = Config
    { minDelayMs :: Int
    -- ^ Minimum delay between logs (ms)
    , maxDelayMs :: Int
    -- ^ Maximum delay between logs (ms)
    , errorProbability :: Double
    -- ^ Probability of generating an error (0.0-1.0)
    }

defaultConfig :: Config
defaultConfig =
    Config
        { minDelayMs = 100
        , maxDelayMs = 2000
        , errorProbability = 0.15 -- 15% chance of error
        }

main :: IO ()
main = do
    putStrLn "-- Log Generator started --"
    hFlush stdout
    forever $ do
        generateAndEmitLog defaultConfig
        delay <- randomRIO (minDelayMs defaultConfig, maxDelayMs defaultConfig)
        threadDelay (delay * 1000) -- threadDelay takes microseconds

generateAndEmitLog :: Config -> IO ()
generateAndEmitLog config = do
    roll <- randomRIO (0.0, 1.0) :: IO Double
    if roll < errorProbability config
        then generateErrorLog
        else generateNormalLog

generateNormalLog :: IO ()
generateNormalLog = do
    timestamp <- getTimestamp
    levelIdx <- randomRIO (0, 2) :: IO Int
    let level = [DEBUG, INFO, WARN] !! levelIdx
    msgIdx <- randomRIO (0, length normalMessages - 1)
    let msg = normalMessages !! msgIdx
    putStrLn $ formatLog timestamp level msg
    hFlush stdout

generateErrorLog :: IO ()
generateErrorLog = do
    errorTypeIdx <- randomRIO (0, 2) :: IO Int
    let errorType = [PythonError, JavaError, NodeError] !! errorTypeIdx
    case errorType of
        PythonError -> generatePythonError
        JavaError -> generateJavaError
        NodeError -> generateNodeError

-- Python error generation
generatePythonError :: IO ()
generatePythonError = do
    timestamp <- getTimestamp
    errorIdx <- randomRIO (0, length pythonErrors - 1)
    let (errorMsg, traceback) = pythonErrors !! errorIdx
    putStrLn $ formatLog timestamp ERROR errorMsg
    mapM_ putStrLn traceback
    hFlush stdout

-- Java error generation
generateJavaError :: IO ()
generateJavaError = do
    timestamp <- getTimestamp
    errorIdx <- randomRIO (0, length javaErrors - 1)
    let (errorMsg, stacktrace) = javaErrors !! errorIdx
    putStrLn $ formatJavaLog timestamp ERROR errorMsg
    mapM_ putStrLn stacktrace
    hFlush stdout

-- Node.js error generation
generateNodeError :: IO ()
generateNodeError = do
    timestamp <- getTimestamp
    errorIdx <- randomRIO (0, length nodeErrors - 1)
    let (errorMsg, stacktrace) = nodeErrors !! errorIdx
    putStrLn $ formatNodeLog timestamp ERROR errorMsg
    mapM_ putStrLn stacktrace
    hFlush stdout

-- Timestamp formatting
getTimestamp :: IO String
getTimestamp = do
    now <- getCurrentTime
    return $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now

formatLog :: String -> LogLevel -> String -> String
formatLog ts level msg = ts ++ " " ++ show level ++ " " ++ msg

formatJavaLog :: String -> LogLevel -> String -> String
formatJavaLog ts level msg =
    ts ++ ".123 [main] " ++ show level ++ " com.example.Application - " ++ msg

formatNodeLog :: String -> LogLevel -> String -> String
formatNodeLog ts level msg =
    "[" ++ ts ++ ".123Z] " ++ show level ++ ": " ++ msg

-- Normal log messages
normalMessages :: [String]
normalMessages =
    [ "Processing request from client"
    , "Database query completed successfully"
    , "Cache hit for session data"
    , "User authentication successful"
    , "API rate limit check passed"
    , "Background job scheduled"
    , "Health check passed"
    , "Configuration reloaded"
    , "Connection pool size: 10"
    , "Memory usage within limits"
    , "Request processed in 45ms"
    , "Starting batch processing"
    , "Batch completed: 100 records"
    , "Webhook delivered successfully"
    , "Session created for user"
    , "File upload completed"
    , "Email queued for delivery"
    , "Metrics exported to monitoring"
    , "SSL certificate valid"
    , "Service discovery updated"
    ]

-- Python errors with tracebacks
pythonErrors :: [(String, [String])]
pythonErrors =
    [
        ( "Failed to process user request"
        ,
            [ "Traceback (most recent call last):"
            , "  File \"/app/handlers/user_handler.py\", line 45, in process_request"
            , "    result = self.fetch_user_data(user_id)"
            , "  File \"/app/services/user_service.py\", line 89, in fetch_user_data"
            , "    return data['user']['profile']['email'].lower()"
            , "KeyError: 'profile'"
            ]
        )
    ,
        ( "Database connection error"
        ,
            [ "Traceback (most recent call last):"
            , "  File \"/app/db/connection.py\", line 178, in execute_query"
            , "    cursor.execute(query, params)"
            , "psycopg2.OperationalError: server closed the connection unexpectedly"
            , "\tThis probably means the server terminated abnormally"
            , "\tbefore or while processing the request."
            ]
        )
    ,
        ( "Type mismatch in calculation"
        ,
            [ "Traceback (most recent call last):"
            , "  File \"/app/analytics/calculator.py\", line 56, in calculate_metrics"
            , "    total = sum(values) / len(values)"
            , "  File \"/app/analytics/calculator.py\", line 45, in <lambda>"
            , "    values = [int(x) for x in raw_values]"
            , "TypeError: int() argument must be a string or a real number, not 'NoneType'"
            ]
        )
    ,
        ( "AttributeError in payment service"
        ,
            [ "Traceback (most recent call last):"
            , "  File \"/app/services/payment_service.py\", line 223, in process_payment"
            , "    return payment.get_transaction_id()"
            , "AttributeError: 'NoneType' object has no attribute 'get_transaction_id'"
            ]
        )
    ,
        ( "Import error during module load"
        ,
            [ "Traceback (most recent call last):"
            , "  File \"/app/main.py\", line 12, in <module>"
            , "    from services.external_api import ExternalClient"
            , "  File \"/app/services/external_api.py\", line 5, in <module>"
            , "    import missing_dependency"
            , "ModuleNotFoundError: No module named 'missing_dependency'"
            ]
        )
    ]

-- Java errors with stack traces
javaErrors :: [(String, [String])]
javaErrors =
    [
        ( "Failed to fetch user details"
        ,
            [ "java.lang.NullPointerException: Cannot invoke \"com.example.model.User.getProfile()\" because \"user\" is null"
            , "\tat com.example.service.UserService.getUserDetails(UserService.java:145)"
            , "\tat com.example.controller.UserController.getUser(UserController.java:67)"
            , "\tat java.base/jdk.internal.reflect.NativeMethodAccessorImpl.invoke0(Native Method)"
            , "\tat java.base/jdk.internal.reflect.NativeMethodAccessorImpl.invoke(NativeMethodAccessorImpl.java:77)"
            , "\tat org.springframework.web.servlet.mvc.method.annotation.RequestMappingHandlerAdapter.handleInternal(RequestMappingHandlerAdapter.java:808)"
            ]
        )
    ,
        ( "Payment processing failed"
        ,
            [ "java.lang.IllegalArgumentException: Payment amount must be positive"
            , "\tat com.example.service.PaymentService.validateAmount(PaymentService.java:89)"
            , "\tat com.example.service.PaymentService.processPayment(PaymentService.java:134)"
            , "\tat com.example.service.OrderService.createOrder(OrderService.java:201)"
            , "\tat com.example.controller.OrderController.placeOrder(OrderController.java:78)"
            ]
        )
    ,
        ( "Database connection error during sync"
        ,
            [ "java.sql.SQLException: Connection is closed"
            , "\tat org.postgresql.jdbc.PgConnection.checkClosed(PgConnection.java:856)"
            , "\tat org.postgresql.jdbc.PgConnection.createStatement(PgConnection.java:1612)"
            , "\tat com.zaxxer.hikari.pool.ProxyConnection.createStatement(ProxyConnection.java:321)"
            , "\tat com.example.repository.ProductRepository.findAll(ProductRepository.java:45)"
            , "\tat com.example.jobs.DataSyncJob.syncProducts(DataSyncJob.java:89)"
            ]
        )
    ,
        ( "JSON parsing failed"
        ,
            [ "com.fasterxml.jackson.core.JsonParseException: Unexpected character ('}' (code 125)): was expecting double-quote to start field name"
            , " at [Source: (String)\"{\"name\": \"Product\", \"price\": 29.99, }\"; line: 1, column: 40]"
            , "\tat com.fasterxml.jackson.core.JsonParser._constructError(JsonParser.java:2337)"
            , "\tat com.fasterxml.jackson.core.base.ParserMinimalBase._reportError(ParserMinimalBase.java:720)"
            , "\tat com.example.util.JsonParser.parse(JsonParser.java:34)"
            ]
        )
    ,
        ( "Failed to send notification email"
        ,
            [ "javax.mail.MessagingException: Could not connect to SMTP host: mail.example.com, port: 587"
            , "\tat com.sun.mail.smtp.SMTPTransport.openServer(SMTPTransport.java:2212)"
            , "\tat com.sun.mail.smtp.SMTPTransport.protocolConnect(SMTPTransport.java:740)"
            , "\tat javax.mail.Service.connect(Service.java:388)"
            , "\tat com.example.service.EmailService.sendEmail(EmailService.java:78)"
            , "Caused by: java.net.ConnectException: Connection refused"
            , "\tat java.base/sun.nio.ch.Net.pollConnect(Native Method)"
            ]
        )
    ]

-- Node.js errors with stack traces
nodeErrors :: [(String, [String])]
nodeErrors =
    [
        ( "Unhandled promise rejection"
        ,
            [ "UnhandledPromiseRejectionWarning: Error: Failed to fetch user data"
            , "    at UserController.getUser (/app/controllers/userController.js:45:15)"
            , "    at processTicksAndRejections (node:internal/process/task_queues:96:5)"
            , "    at async /app/middleware/asyncHandler.js:12:9"
            ]
        )
    ,
        ( "TypeError in payment processing"
        ,
            [ "TypeError: Cannot read property 'amount' of undefined"
            , "    at PaymentService.processPayment (/app/services/paymentService.js:78:32)"
            , "    at OrderController.createOrder (/app/controllers/orderController.js:156:24)"
            , "    at Layer.handle [as handle_request] (/app/node_modules/express/lib/router/layer.js:95:5)"
            , "    at next (/app/node_modules/express/lib/router/route.js:137:13)"
            , "    at Route.dispatch (/app/node_modules/express/lib/router/route.js:112:3)"
            ]
        )
    ,
        ( "ReferenceError in analytics module"
        ,
            [ "ReferenceError: analytics is not defined"
            , "    at trackEvent (/app/utils/analytics.js:23:5)"
            , "    at ProductController.listProducts (/app/controllers/productController.js:89:9)"
            , "    at Layer.handle [as handle_request] (/app/node_modules/express/lib/router/layer.js:95:5)"
            ]
        )
    ,
        ( "Database query failed"
        ,
            [ "Error: Connection terminated unexpectedly"
            , "    at Connection.parseE (/app/node_modules/pg/lib/connection.js:614:13)"
            , "    at Connection.parseMessage (/app/node_modules/pg/lib/connection.js:413:19)"
            , "    at Socket.<anonymous> (/app/node_modules/pg/lib/connection.js:129:22)"
            , "    at Socket.emit (node:events:390:28)"
            , "    at TCP.onStreamRead (node:internal/stream_base_commons:190:23)"
            ]
        )
    ,
        ( "ENOENT: no such file or directory"
        ,
            [ "Error: ENOENT: no such file or directory, open '/tmp/uploads/missing-file.jpg'"
            , "    at Object.openSync (node:fs:590:3)"
            , "    at Object.readFileSync (node:fs:458:35)"
            , "    at FileService.processUpload (/app/services/fileService.js:67:23)"
            , "    at UploadController.handleUpload (/app/controllers/uploadController.js:89:34)"
            ]
        )
    ]
